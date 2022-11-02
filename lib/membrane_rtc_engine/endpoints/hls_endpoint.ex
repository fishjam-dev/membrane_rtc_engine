if Enum.all?(
     [
       Membrane.H264.FFmpeg.Parser,
       Membrane.HTTPAdaptiveStream.SinkBin
     ],
     &Code.ensure_loaded?/1
   ) do
  defmodule Membrane.RTC.Engine.Endpoint.HLS do
    @moduledoc """
    An Endpoint responsible for converting incoming tracks to HLS playlist.

    This module requires the following plugins to be present in your `mix.exs` for H264 & AAC input:
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
      :membrane_aac_fdk_plugin,
    ]
    ```
    """
    use Membrane.Bin

    require Membrane.Logger

    alias Membrane.RTC.Engine
    alias Membrane.RTC.Engine.Endpoint.HLS.{AudioMixerConfig, CompositorConfig}

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

    def_options(
      rtc_engine: [
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
      target_segment_duration: [
        type: :time,
        spec: Membrane.Time.t(),
        default: Membrane.Time.seconds(5),
        description: """
        Expected length of each segment. Setting it is not necessary, but
        may help players achieve better UX.
        """
      ],
      mixer_config: [
        spec: %{audio: AudioMixerConfig.t(), video: CompositorConfig.t()} | nil,
        default: nil,
        description: """
        Audio and video mixer configuration.
        """
      ]
    )

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
        target_segment_duration: opts.target_segment_duration,
        mixer_config: opts.mixer_config
      }

      {:ok, state}
    end

    @impl true
    def handle_other({:new_tracks, tracks}, ctx, state) do
      {:endpoint, endpoint_id} = ctx.name
      tracks = Enum.filter(tracks, fn track -> :raw in track.format end)

      state =
        Enum.reduce(tracks, state, fn track, state ->
          case Engine.subscribe(state.rtc_engine, endpoint_id, track.id, :raw) do
            :ok ->
              put_in(state, [:tracks, track.id], track)

            {:error, :invalid_track_id} ->
              Membrane.Logger.debug("""
              Couldn't subscribe to track: #{inspect(track.id)}. No such track.
              It had to be removed just after publishing it. Ignoring.
              """)

              state

            {:error, reason} ->
              raise "Couldn't subscribe for track: #{inspect(track.id)}. Reason: #{inspect(reason)}"
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
      send(
        state.owner,
        {:playlist_playable, content_type, state.mixer_config.video.output_directory}
      )

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
          :hls_sink_bin,
          _ctx,
          state
        ) do
      send(state.owner, {:cleanup, clean_function, state.mixer_config.video.output_directory})
      {:ok, state}
    end

    def handle_notification(
          {:cleanup, clean_function},
          {:hls_sink_bin, stream_id},
          _ctx,
          state
        ) do
      send(state.owner, {:cleanup, clean_function, stream_id})
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
          :keyframe_requester,
          :video_parser,
          :video_parser_out,
          :decoder,
          :encoder,
          :resolution_scaler,
          :framerate_converter
        ]
        |> Enum.map(&{&1, track_id})
        |> Enum.filter(&Map.has_key?(ctx.children, &1))

      {removed_track, tracks} = Map.pop!(state.tracks, track_id)
      state = %{state | tracks: tracks}

      children_to_remove =
        if is_nil(state.mixer_config) do
          sink_bin_used? =
            Enum.any?(tracks, fn {_id, track} ->
              track.stream_id == removed_track.stream_id
            end)

          if sink_bin_used?,
            do: track_children,
            else: [{:hls_sink_bin, removed_track.stream_id} | track_children]
        else
          track_children ++ if tracks == %{}, do: get_common_children(), else: []
        end

      {{:ok, remove_child: children_to_remove}, state}
    end

    defp get_common_children(),
      do: [
        :compositor,
        :encoder,
        :video_parser_out,
        :hls_sink_bin,
        :audio_mixer,
        :aac_encoder,
        :aac_parser
      ]

    @impl true
    def handle_pad_added(Pad.ref(:input, track_id) = pad, ctx, state) do
      link_builder = link_bin_input(pad)
      track = Map.get(state.tracks, track_id)
      directory = choose_hls_stream_directory(state, track)
      spec = hls_links_and_children(link_builder, track, state, ctx)

      {spec, state} =
        if (Map.has_key?(ctx.children, :hls_sink_bin) && !is_nil(state.mixer_config)) ||
             Map.has_key?(ctx.children, {:hls_sink_bin, track.stream_id}) do
          {spec, state}
        else
          # remove directory if it already exists
          File.rm_rf(directory)
          File.mkdir_p!(directory)

          hls_sink_spec = choose_hls_sink_spec(state, track, directory)

          {merge_parent_specs(spec, hls_sink_spec), state}
        end

      {{:ok, spec: spec}, state}
    end

    defp create_hls_sink(directory, state),
      do: %Membrane.HTTPAdaptiveStream.SinkBin{
        manifest_module: Membrane.HTTPAdaptiveStream.HLS,
        target_window_duration: state.target_window_duration,
        target_segment_duration: state.target_segment_duration,
        muxer_segment_duration: state.target_segment_duration - Membrane.Time.seconds(1),
        persist?: false,
        storage: %Membrane.HTTPAdaptiveStream.Storages.FileStorage{
          directory: directory
        },
        hls_mode: state.hls_mode
      }

    defp choose_hls_sink_spec(%{mixer_config: nil} = state, track, directory),
      do: %ParentSpec{
        children: %{
          {:hls_sink_bin, track.stream_id} => create_hls_sink(directory, state)
        }
      }

    defp choose_hls_sink_spec(state, _track, directory),
      do: %ParentSpec{
        children: %{
          hls_sink_bin: create_hls_sink(directory, state)
        }
      }

    defp choose_hls_stream_directory(state, track) do
      if !is_nil(state.mixer_config) && !is_nil(state.mixer_config.video.output_directory) do
        Path.join(state.output_directory, state.mixer_config.video.output_directory)
      else
        Path.join(state.output_directory, track.stream_id)
      end
    end

    defp merge_parent_specs(spec1, spec2) do
      %ParentSpec{
        children: Map.merge(spec1.children, spec2.children),
        links: spec1.links ++ spec2.links
      }
    end

    defp hls_links_and_children(
           link_builder,
           %{encoding: :OPUS} = track,
           %{mixer_config: nil},
           _ctx
         ) do
      %ParentSpec{
        children: %{
          {:opus_decoder, track.id} => Membrane.Opus.Decoder,
          {:aac_encoder, track.id} => Membrane.AAC.FDK.Encoder,
          {:aac_parser, track.id} => %Membrane.AAC.Parser{out_encapsulation: :none}
        },
        links: [
          link_builder
          |> to({:opus_decoder, track.id})
          |> to({:aac_encoder, track.id})
          |> to({:aac_parser, track.id})
          |> via_in(Pad.ref(:input, {:audio, track.id}), options: [encoding: :AAC])
          |> to({:hls_sink_bin, track.stream_id})
        ]
      }
    end

    defp hls_links_and_children(
           link_builder,
           %{encoding: :AAC} = track,
           %{mixer_config: nil},
           _ctx
         ) do
      %ParentSpec{
        children: %{},
        links: [
          link_builder
          |> via_in(Pad.ref(:input, {:audio, track.id}), options: [encoding: :AAC])
          |> to({:hls_sink_bin, track.stream_id})
        ]
      }
    end

    defp hls_links_and_children(link_builder, %{encoding: :OPUS} = track, state, ctx) do
      parent_spec = %ParentSpec{
        children: %{
          {:opus_decoder, track.id} => Membrane.Opus.Decoder
        },
        links: [
          link_builder
          |> to({:opus_decoder, track.id})
          |> to(:audio_mixer)
        ]
      }

      state
      |> generate_audio_mixer(ctx)
      |> merge_parent_specs(parent_spec)
    end

    if Enum.all?(@audio_mixer_deps, &Code.ensure_loaded?/1) do
      defp hls_links_and_children(link_builder, %{encoding: :AAC}, state, ctx) do
        parent_spec = %ParentSpec{
          links: [
            link_builder
            |> to(:audio_mixer)
          ]
        }

        state
        |> generate_audio_mixer(ctx)
        |> merge_parent_specs(parent_spec)
      end
    else
      defp hls_links_and_children(link_builder, %{encoding: :AAC}, state, ctx) do
        raise """
        Cannot find some of the modules required to use the audio mixer.
        Ensure that the following dependencies are added to the deps.
        #{merge_strings(@audio_mixer_deps)}
        """
      end

      defp merge_strings(strings),
        do: strings |> Enum.map(String.replace_suffix("", " ")) |> Enum.reduce(Kernel.<>())
    end

    defp hls_links_and_children(
           link_builder,
           %{encoding: :H264} = track,
           %{mixer_config: nil} = state,
           _ctx
         ) do
      %ParentSpec{
        children: %{
          {:keyframe_requester, track.id} => %Membrane.KeyframeRequester{
            interval: state.target_segment_duration
          },
          {:video_parser, track.id} => %Membrane.H264.FFmpeg.Parser{
            alignment: :au,
            # jest jakis powod dla ktorego zrezygnowalismy z framerate'u
            attach_nalus?: true
          }
        },
        links: [
          link_builder
          |> to({:keyframe_requester, track.id})
          |> to({:video_parser, track.id})
          |> via_in(Pad.ref(:input, {:video, track.id}), options: [encoding: :H264])
          |> to({:hls_sink_bin, track.stream_id})
        ]
      }
    end

    if Enum.all?(@compositor_deps, &Code.ensure_loaded?/1) do
      defp hls_links_and_children(link_builder, %{encoding: :H264} = track, state, ctx) do
        parent_spec = %ParentSpec{
          children: %{
            {:keyframe_requester, track.id} => %Membrane.KeyframeRequester{
              interval: state.target_segment_duration
            },
            {:video_parser, track.id} => %Membrane.H264.FFmpeg.Parser{
              alignment: :au,
              attach_nalus?: true
            },
            {:framerate_converter, track.id} => %Membrane.FramerateConverter{
              framerate: state.mixer_config.video.output_framerate
            }
          },
          links: [
            link_builder
            |> to({:keyframe_requester, track.id})
            |> to({:video_parser, track.id})
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

      defp merge_strings(strings),
        do: strings |> Enum.map(String.replace_suffix("", " ")) |> Enum.reduce(Kernel.<>())
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
      seconds_number = Membrane.Time.as_seconds(state.target_segment_duration)

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
          |> via_in(Pad.ref(:input, :video), options: [encoding: :H264])
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
          |> via_in(Pad.ref(:input, :audio), options: [encoding: :AAC])
          |> to(:hls_sink_bin)
        ]
      }
    end
  end
end
