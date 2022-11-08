if Enum.all?(
     [
       Membrane.H264.FFmpeg.Parser,
       Membrane.HTTPAdaptiveStream.SinkBin,
       Membrane.Opus.Decoder,
       Membrane.AAC.Parser,
       Membrane.AAC.FDK.Encoder
     ],
     &Code.ensure_loaded?/1
   ) do
  defmodule Membrane.RTC.Engine.Endpoint.HLS do
    @moduledoc """
    An Endpoint responsible for converting incoming tracks to HLS playlist.

    This module requires the following plugins to be present in your `mix.exs` for H264 & OPUS input:
    ```
    [
      :membrane_h264_ffmpeg_plugin,
      :membrane_http_adaptive_stream_plugin,
    ]
    ```

    It can perform transcoding (see `Membrane.RTC.Engine.Endpoint.HLS.TranscodingConfig`),
    in such case these plugins are also needed:
    ```
    [
      :membrane_ffmpeg_swscale_plugin,
      :membrane_framerate_converter_plugin
    ]
    ```

    Plus, optionally it supports OPUS audio input - for that, additional dependencies are needed:
    ```
    [
      :membrane_opus_plugin,
      :membrane_aac_plugin,
      :membrane_aac_fdk_plugin
    ]
    ```
    """
    use Membrane.Bin

    require Membrane.Logger

    alias Membrane.HTTPAdaptiveStream.Sink.SegmentDuration
    alias Membrane.RTC.Engine
    alias Membrane.RTC.Engine.Endpoint.HLS.{AudioMixerConfig, CompositorConfig}
    alias Membrane.RTC.Engine.Endpoint.HLS.TrackSynchronizer
    alias Membrane.RTC.Engine.Endpoint.WebRTC.TrackReceiver
    alias Membrane.RTC.Engine.Track
    alias Membrane.Time

    @compositor_deps [
      Membrane.H264.FFmpeg.Decoder,
      Membrane.H264.FFmpeg.Encoder,
      Membrane.FFmpeg.SWScale.Scaler,
      Membrane.FramerateConverter,
      Membrane.VideoMixer
    ]
    @audio_mixer_deps [
      Membrane.AudioMixer,
      Membrane.AAC.Parser,
      Membrane.AAC.FDK.Encoder
    ]

    def_input_pad(:input,
      demand_unit: :buffers,
      caps: :any,
      availability: :on_request
    )

    def_options rtc_engine: [
                  spec: pid(),
                  description: "Pid of parent Engine"
                ],
                output_directory: [
                  spec: Path.t(),
                  description: "Path to directory under which HLS output will be saved",
                  default: "hls_output"
                ],
                owner: [
                  spec: pid(),
                  description: """
                  Pid of parent all notifications will be send to.

                  These notifications are:
                    * `{:playlist_playable, content_type}`
                    * `{:cleanup, clean_function}`
                  """
                ],
                hls_mode: [
                  spec: :separate_av | :muxed_av,
                  default: :separate_av,
                  description: """
                  Defines output mode for `Membrane.HTTPAdaptiveStream.SinkBin`.

                  - `:separate_av` - audio and video tracks will be separated
                  - `:muxed_av` - audio will be attached to every video track
                  """
                ],
                target_window_duration: [
                  type: :time,
                  spec: Membrane.Time.t() | :infinity,
                  default: Membrane.Time.seconds(20),
                  description: """
                  Max duration of stream that will be stored. Segments that are older than window duration will be removed.
                  """
                ],
                segment_duration: [
                  spec: SegmentDuration.t(),
                  default: SegmentDuration.new(Time.seconds(4), Time.seconds(5)),
                  description: """
                  Expected length of each segment. Setting it is not necessary, but
                  may help players achieve better UX.
                  """
                ],
                mixer_config: [
                  spec: %{audio: AudioMixerConfig.t(), video: CompositorConfig.t()} | nil,
                  default: nil,
                  description: """
                  Audio and video mixer configuration. If you don't want to use compositor pass nil.
                  """
                ],
                broadcast_mode: [
                  spec: :live | :vod,
                  default: :live,
                  description: """
                  Tells if the session is live or a VOD type of broadcast. It can influence type of metadata
                  inserted into the playlist's manifest.
                  """
                ]

    @impl true
    def handle_init(opts) do
      state = %{
        rtc_engine: opts.rtc_engine,
        tracks: %{},
        stream_ids: MapSet.new(),
        output_directory: opts.output_directory,
        owner: opts.owner,
        hls_mode: opts.hls_mode,
        target_window_duration: opts.target_window_duration,
        segment_duration: opts.segment_duration,
        mixer_config: opts.mixer_config,
        broadcast_mode: opts.broadcast_mode
      }

      {:ok, state}
    end

    @impl true
    def handle_other({:new_tracks, tracks}, ctx, state) do
      {:endpoint, endpoint_id} = ctx.name

      state =
        Enum.reduce(tracks, state, fn track, state ->
          case Engine.subscribe(state.rtc_engine, endpoint_id, track.id) do
            :ok ->
              put_in(state, [:tracks, track.id], track)

            {:error, :invalid_track_id} ->
              Membrane.Logger.debug("""
              Couldn't subscribe to the track: #{inspect(track.id)}. No such track.
              It had to be removed just after publishing it. Ignoring.
              """)

              state

            {:error, reason} ->
              raise "Couldn't subscribe to the track: #{inspect(track.id)}. Reason: #{inspect(reason)}"
          end
        end)

      {:ok, state}
    end

    @impl true
    def handle_other(msg, _ctx, state) do
      Membrane.Logger.warn("Unexpected message: #{inspect(msg)}. Ignoring.")
      {:ok, state}
    end

    def handle_notification(
          {:track_playable, content_type},
          :hls_sink_bin,
          _ctx,
          state
        ) do
      send(state.owner, {:playlist_playable, content_type, ""})
      {:ok, state}
    end

    def handle_notification(
          {:track_playable, {content_type, _track_id}},
          {:hls_sink_bin, stream_id},
          _ctx,
          state
        ) do
      send(state.owner, {:playlist_playable, content_type, stream_id})
      {:ok, state}
    end

    def handle_notification(
          {:cleanup, clean_function},
          children,
          _ctx,
          state
        ) do
      case children do
        :hls_sink_bin ->
          send(state.owner, {:cleanup, clean_function, ""})

        {:hls_sink_bin, stream_id} ->
          send(state.owner, {:cleanup, clean_function, stream_id})
      end

      {:ok, state}
    end

    @impl true
    def handle_notification(notification, _element, _context, state) do
      Membrane.Logger.warn("Unexpected notification: #{inspect(notification)}. Ignoring.")
      {:ok, state}
    end

    @impl true
    def handle_pad_removed(Pad.ref(:input, track_id), ctx, state) do
      track_children =
        [
          :opus_decoder,
          :aac_encoder,
          :aac_parser,
          :video_parser,
          :decoder,
          :framerate_converter,
          :track_receiver,
          :depayloader,
          :track_sync
        ]
        |> Enum.map(&{&1, track_id})
        |> Enum.filter(&Map.has_key?(ctx.children, &1))

      {removed_track, tracks} = Map.pop!(state.tracks, track_id)
      state = %{state | tracks: tracks}

      sink_bin_used? =
        Enum.any?(tracks, fn {_id, track} ->
          track.stream_id == removed_track.stream_id
        end)

      children_to_remove =
        cond do
          is_nil(state.mixer_config) and not sink_bin_used? ->
            [{:hls_sink_bin, removed_track.stream_id}]

          not is_nil(state.mixer_config) and tracks == %{} ->
            get_common_children(ctx)

          true ->
            []
        end

      children_to_remove = track_children ++ children_to_remove

      {{:ok, remove_child: children_to_remove}, state}
    end

    @impl true
    def handle_pad_added(Pad.ref(:input, track_id) = pad, ctx, %{hls_mode: :separate_av} = state) do
      link_builder = link_bin_input(pad)
      track = Map.get(state.tracks, track_id)
      directory = get_hls_stream_directory(state, track)

      spec =
        get_initial_spec(link_builder, track, state)
        |> merge_parent_specs(hls_links_and_children(track, state, ctx))

      {spec, state} =
        if hls_sink_bin_exists?(track, ctx, state) do
          {spec, state}
        else
          new_spec = maybe_add_hls_sink_bin(directory, spec, track, ctx, state)

          {new_spec, state}
        end

      {{:ok, spec: spec}, state}
    end

    @impl true
    def handle_pad_added(Pad.ref(:input, track_id) = pad, ctx, %{hls_mode: :muxed_av} = state) do
      link_builder = link_bin_input(pad)
      track = Map.get(state.tracks, track_id)
      directory = get_hls_stream_directory(state, track)

      initial_spec = get_initial_spec(link_builder, track, state)

      {spec, state} =
        if MapSet.member?(state.stream_ids, track.stream_id) do
          initial_spec = maybe_add_hls_sink_bin(directory, initial_spec, track, ctx, state)
          {hls_link_both_tracks(initial_spec, track, ctx, state), state}
        else
          spec = %ParentSpec{
            children: %{
              {:track_sync, track.stream_id} => TrackSynchronizer
            },
            links: []
          }

          spec = merge_parent_specs(initial_spec, spec)
          {spec, %{state | stream_ids: MapSet.put(state.stream_ids, track.stream_id)}}
        end

      {{:ok, spec: spec}, state}
    end

    defp get_initial_spec(link_builder, track, state) do
      link =
        case {track.type, state.hls_mode} do
          {_, :muxed_av} ->
            [
              link({:depayloader, track.id})
              |> via_in(Pad.ref(:input, track.type))
              |> to({:track_sync, track.stream_id})
            ]

          {:audio, _} ->
            [
              link({:depayloader, track.id})
              |> to({:opus_decoder, track.id})
            ]

          {:video, _} ->
            [
              link({:depayloader, track.id})
              |> to({:video_parser, track.id})
            ]
        end

      %ParentSpec{
        children: %{
          {:track_receiver, track.id} => %TrackReceiver{
            track: track,
            initial_target_variant: :high,
            keyframe_request_interval: state.segment_duration.target
          },
          {:depayloader, track.id} => get_depayloader(track)
        },
        links:
          [
            link_builder
            |> to({:track_receiver, track.id})
            |> to({:depayloader, track.id})
          ] ++ link
      }
    end

    if Enum.all?(@audio_mixer_deps, &Code.ensure_loaded?/1) and
         Enum.all?(@compositor_deps, &Code.ensure_loaded?/1) do
      defp hls_link_both_tracks(initial_spec, track, ctx, state) do
        {audio_track, video_track} = get_audio_video_tracks(track, state)
        audio_spec = hls_links_and_children(audio_track, state, ctx)
        video_spec = hls_links_and_children(video_track, state, ctx)
        mixer_spec = merge_parent_specs(audio_spec, video_spec)

        sync_spec = %ParentSpec{
          links: [
            link({:track_sync, track.stream_id})
            |> via_out(Pad.ref(:output, :audio))
            |> to({:opus_decoder, audio_track.id}),
            link({:track_sync, track.stream_id})
            |> via_out(Pad.ref(:output, :video))
            |> to({:video_parser, video_track.id})
          ]
        }

        merge_parent_specs(initial_spec, sync_spec)
        |> merge_parent_specs(mixer_spec)
      end
    else
      defp hls_link_both_tracks(initial_spec, track, ctx, state) do
        raise """
        Cannot find some of the modules required to use the audio and video mixer.
        Ensure that the following dependencies are added to the deps.
        #{merge_strings(@audio_mixer_deps)}, #{merge_strings(@compositor_deps)}
        """
      end
    end

    defp hls_links_and_children(
           %{encoding: :OPUS} = track,
           %{mixer_config: nil} = state,
           _ctx
         ) do
      %ParentSpec{
        children: %{
          {:opus_decoder, track.id} => Membrane.Opus.Decoder,
          {:aac_encoder, track.id} => Membrane.AAC.FDK.Encoder,
          {:aac_parser, track.id} => %Membrane.AAC.Parser{out_encapsulation: :none}
        },
        links: [
          link({:opus_decoder, track.id})
          |> to({:aac_encoder, track.id})
          |> to({:aac_parser, track.id})
          |> via_in(Pad.ref(:input, {:audio, track.id}),
            options: [encoding: :AAC, segment_duration: state.segment_duration]
          )
          |> to({:hls_sink_bin, track.stream_id})
        ]
      }
    end

    if Enum.all?(@audio_mixer_deps, &Code.ensure_loaded?/1) do
      defp hls_links_and_children(%{encoding: :OPUS} = track, state, ctx) do
        parent_spec = %ParentSpec{
          children: %{
            {:opus_decoder, track.id} => Membrane.Opus.Decoder
          },
          links: [
            link({:opus_decoder, track.id})
            |> to(:audio_mixer)
          ]
        }

        state
        |> generate_audio_mixer(ctx)
        |> merge_parent_specs(parent_spec)
      end
    else
      defp hls_links_and_children(%{encoding: :OPUS}, state, ctx) do
        raise """
        Cannot find some of the modules required to use the audio mixer.
        Ensure that the following dependencies are added to the deps.
        #{merge_strings(@audio_mixer_deps)}
        """
      end
    end

    defp hls_links_and_children(
           %{encoding: :H264} = track,
           %{mixer_config: nil} = state,
           _ctx
         ) do
      %ParentSpec{
        children: %{
          {:video_parser, track.id} => %Membrane.H264.FFmpeg.Parser{
            alignment: :au,
            attach_nalus?: true
          }
        },
        links: [
          link({:video_parser, track.id})
          |> via_in(Pad.ref(:input, {:video, track.id}),
            options: [encoding: :H264, segment_duration: state.segment_duration]
          )
          |> to({:hls_sink_bin, track.stream_id})
        ]
      }
    end

    if Enum.all?(@compositor_deps, &Code.ensure_loaded?/1) do
      defp hls_links_and_children(%{encoding: :H264} = track, state, ctx) do
        parent_spec = %ParentSpec{
          children: %{
            {:video_parser, track.id} => %Membrane.H264.FFmpeg.Parser{
              alignment: :au,
              attach_nalus?: true
            },
            {:framerate_converter, track.id} => %Membrane.FramerateConverter{
              framerate: state.mixer_config.video.output_framerate
            }
          },
          links: [
            link({:video_parser, track.id})
            |> to({:decoder, track.id}, Membrane.H264.FFmpeg.Decoder)
            |> to({:framerate_converter, track.id})
            |> to(:compositor)
          ]
        }

        state
        |> generate_compositor(ctx)
        |> merge_parent_specs(parent_spec)
      end
    else
      defp hls_links_and_children(
             _link_builder,
             %{encoding: :H264} = track,
             _segment_duration,
             _framerate
           ) do
        raise """
        Cannot find some of the modules required to use the video composer.
        Ensure that the following dependencies are added to the deps.
        #{merge_strings(@compositor_deps)}
        """
      end
    end

    defp maybe_add_hls_sink_bin(_directory, initial_spec, _track, ctx, state)
         when is_map_key(ctx.children, :hls_sink_bin) and not is_nil(state.mixer_config),
         do: initial_spec

    defp maybe_add_hls_sink_bin(directory, initial_spec, track, _ctx, state) do
      # remove directory if it already exists
      File.rm_rf(directory)
      File.mkdir_p!(directory)

      hls_sink_bin = %Membrane.HTTPAdaptiveStream.SinkBin{
        manifest_module: Membrane.HTTPAdaptiveStream.HLS,
        target_window_duration: state.target_window_duration,
        persist?: false,
        storage: %Membrane.HTTPAdaptiveStream.Storages.FileStorage{
          directory: directory
        },
        hls_mode: state.hls_mode,
        mode: state.broadcast_mode,
        mp4_parameters_in_band?: is_nil(state.mixer_config)
      }

      children =
        if is_nil(state.mixer_config),
          do: %{{:hls_sink_bin, track.stream_id} => hls_sink_bin},
          else: %{hls_sink_bin: hls_sink_bin}

      merge_parent_specs(initial_spec, %ParentSpec{children: children})
    end

    defp generate_compositor(_state, ctx) when is_map_key(ctx.children, :compositor),
      do: %ParentSpec{children: %{}, links: []}

    defp generate_compositor(state, _ctx) do
      compositor = %Membrane.VideoMixer{
        output_caps: %Membrane.RawVideo{
          width: state.mixer_config.video.output_width,
          height: state.mixer_config.video.output_height,
          pixel_format: :I420,
          framerate: state.mixer_config.video.output_framerate,
          aligned: true
        },
        filters: state.mixer_config.video.ffmpeg_filter
      }

      video_parser_out = %Membrane.H264.FFmpeg.Parser{
        alignment: :au,
        attach_nalus?: true,
        framerate: state.mixer_config.video.output_framerate
      }

      {frames_per_second, 1} = state.mixer_config.video.output_framerate
      seconds_number = Membrane.Time.as_seconds(state.segment_duration.target)

      %ParentSpec{
        children: %{
          compositor: compositor,
          video_parser_out: video_parser_out
        },
        links: [
          link(:compositor)
          |> to(:encoder, %Membrane.H264.FFmpeg.Encoder{
            profile: :baseline,
            gop_size: frames_per_second * seconds_number
          })
          |> to(:video_parser_out)
          |> via_in(Pad.ref(:input, :video),
            options: [encoding: :H264, segment_duration: state.segment_duration]
          )
          |> to(:hls_sink_bin)
        ]
      }
    end

    defp generate_audio_mixer(_state, ctx) when is_map_key(ctx.children, :audio_mixer),
      do: %ParentSpec{children: %{}, links: []}

    defp generate_audio_mixer(state, _ctx) do
      audio_mixer = %Membrane.AudioMixer{
        caps: %Membrane.RawAudio{
          channels: state.mixer_config.audio.channels,
          sample_rate: state.mixer_config.audio.sample_rate,
          sample_format: state.mixer_config.audio.sample_format
        }
      }

      %ParentSpec{
        children: %{
          audio_mixer: audio_mixer,
          aac_encoder: Membrane.AAC.FDK.Encoder,
          aac_parser: %Membrane.AAC.Parser{out_encapsulation: :none}
        },
        links: [
          link(:audio_mixer)
          |> to(:aac_encoder)
          |> to(:aac_parser)
          |> via_in(Pad.ref(:input, :audio),
            options: [encoding: :AAC, segment_duration: state.segment_duration]
          )
          |> to(:hls_sink_bin)
        ]
      }
    end

    defp get_audio_video_tracks(curr_track, state) do
      {_track_id, track} =
        Enum.find(state.tracks, fn {_track_id, track} ->
          track.stream_id == curr_track.stream_id && track.type != curr_track.type
        end)

      if curr_track.type == :audio, do: {curr_track, track}, else: {track, curr_track}
    end

    defp get_hls_stream_directory(%{mixer_config: nil} = state, track),
      do: Path.join(state.output_directory, track.stream_id)

    defp get_hls_stream_directory(state, _track), do: state.output_directory

    defp merge_parent_specs(spec1, spec2) do
      %ParentSpec{
        children: Map.merge(spec1.children, spec2.children),
        links: spec1.links ++ spec2.links
      }
    end

    defp hls_sink_bin_exists?(track, ctx, %{mixer_config: nil}),
      do: Map.has_key?(ctx.children, {:hls_sink_bin, track.stream_id})

    defp hls_sink_bin_exists?(_track, ctx, _state), do: Map.has_key?(ctx.children, :hls_sink_bin)

    defp get_depayloader(track) do
      track
      |> Track.get_depayloader()
      |> tap(&unless &1, do: raise("Couldn't find depayloader for track #{inspect(track)}"))
    end

    defp get_common_children(ctx) do
      children = [
        :compositor,
        :encoder,
        :video_parser_out,
        :hls_sink_bin,
        :audio_mixer,
        :aac_encoder,
        :aac_parser
      ]

      Enum.filter(children, &Map.has_key?(ctx.children, &1))
    end

    if not Enum.all?(@compositor_deps ++ @audio_mixer_deps, &Code.ensure_loaded?/1) do
      defp merge_strings(strings),
        do:
          strings
          |> Enum.map(&String.replace_suffix(&1, "", " "))
          |> Enum.reduce(&Kernel.<>(&1, &2))
    end
  end
end
