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
    alias Membrane.RTC.Engine.Endpoint.HLS.TranscodingConfig
    alias Membrane.RTC.Engine.Endpoint.HLS.TrackSynchronizer

    @opus_deps [Membrane.Opus.Decoder, Membrane.AAC.Parser, Membrane.AAC.FDK.Encoder]
    @transcoding_deps [
      Membrane.H264.FFmpeg.Decoder,
      Membrane.H264.FFmpeg.Encoder,
      Membrane.FFmpeg.SWScale.Scaler,
      Membrane.FramerateConverter
    ]

    def_input_pad :input,
      demand_unit: :buffers,
      caps: :any,
      availability: :on_request

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
                    * `{:playlist_playable, content_type, stream_id, origin}`
                    * `{:cleanup, clean_function, stream_id}`
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
                framerate: [
                  spec: {integer(), integer()} | nil,
                  description: """
                  Framerate of input tracks
                  """,
                  default: nil
                ],
                transcoding_config: [
                  spec: TranscodingConfig.t(),
                  default: %TranscodingConfig{},
                  description: """
                  Transcoding configuration
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
        framerate: opts.framerate,
        target_segment_duration: opts.target_segment_duration,
        transcoding_config: opts.transcoding_config
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
          {:track_playable, {content_type, track_id}},
          {:hls_sink_bin, stream_id},
          _ctx,
          state
        ) do
      %{origin: origin} = Map.fetch!(state.tracks, track_id)
      # notify about playable just when video becomes available
      send(state.owner, {:playlist_playable, content_type, stream_id, origin})
      {:ok, state}
    end

    def handle_notification(
          {:cleanup, clean_function},
          {:hls_sink_bin, stream_id},
          _ctx,
          state
        ) do
      # notify about possibility to cleanup as the stream is finished.
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
      children =
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

      sink_bin_used? =
        Enum.any?(tracks, fn {_id, track} ->
          track.stream_id == removed_track.stream_id
        end)

      children =
        if sink_bin_used?,
          do: children,
          else: [{:hls_sink_bin, removed_track.stream_id} | children]

      {{:ok, remove_child: children}, state}
    end

    @impl true
    def handle_pad_added(Pad.ref(:input, track_id) = pad, _ctx, %{hls_mode: :separate_av} = state) do
      link_builder = link_bin_input(pad)
      track = Map.get(state.tracks, track_id)

      directory = Path.join(state.output_directory, track.stream_id)

      spec =
        hls_links_and_children(
          link_builder,
          track.encoding,
          track,
          state.target_segment_duration,
          state.framerate,
          state.transcoding_config
        )

      {spec, state} =
        if MapSet.member?(state.stream_ids, track.stream_id) do
          {spec, state}
        else
          # remove directory if it already exists
          File.rm_rf(directory)
          File.mkdir_p!(directory)

          hls_sink_bin = %Membrane.HTTPAdaptiveStream.SinkBin{
            manifest_module: Membrane.HTTPAdaptiveStream.HLS,
            target_window_duration: state.target_window_duration,
            target_segment_duration: state.target_segment_duration,
            persist?: false,
            storage: %Membrane.HTTPAdaptiveStream.Storages.FileStorage{
              directory: directory
            },
            hls_mode: state.hls_mode
          }

          new_spec = %{
            spec
            | children: Map.put(spec.children, {:hls_sink_bin, track.stream_id}, hls_sink_bin)
          }

          {new_spec, %{state | stream_ids: MapSet.put(state.stream_ids, track.stream_id)}}
        end

      {{:ok, spec: spec}, state}
    end

    @impl true
    def handle_pad_added(Pad.ref(:input, track_id) = pad, _ctx, %{hls_mode: :muxed_av} = state) do
      link_builder = link_bin_input(pad)
      track = Map.get(state.tracks, track_id)
      directory = Path.join(state.output_directory, track.stream_id)

      link = [
        link_builder
        |> via_in(Pad.ref(:input, track.type))
        |> to({:track_sync, track.stream_id})
      ]

      {spec, state} =
        if MapSet.member?(state.stream_ids, track.stream_id) do
          hls_sink_bin = %Membrane.HTTPAdaptiveStream.SinkBin{
            manifest_module: Membrane.HTTPAdaptiveStream.HLS,
            target_window_duration: state.target_window_duration,
            target_segment_duration: state.target_segment_duration,
            persist?: false,
            storage: %Membrane.HTTPAdaptiveStream.Storages.FileStorage{
              directory: directory
            },
            hls_mode: state.hls_mode
          }

          {hls_link_both_tracks(link, track, hls_sink_bin, state), state}
        else
          # remove directory if it already exists
          File.rm_rf(directory)
          File.mkdir_p!(directory)

          spec = %ParentSpec{
            children: %{
              {:track_sync, track.stream_id} => TrackSynchronizer
            },
            links: link
          }

          {spec, %{state | stream_ids: MapSet.put(state.stream_ids, track.stream_id)}}
        end

      {{:ok, spec: spec}, state}
    end

    defp hls_link_both_tracks(link, track, hls_sink_bin, state) do
      {audio_track, video_track} = get_audio_video_tracks(track, state)

      link_to_transcoder = create_transcoder_link(state.transcoding_config, video_track.id)

      link_to_opus_aac_transcoder = maybe_create_opus_aac_transcoder_link(audio_track)

      %ParentSpec{
        children: %{
          {:hls_sink_bin, track.stream_id} => hls_sink_bin,
          {:keyframe_requester, video_track.id} => %Membrane.KeyframeRequester{
            interval: state.target_segment_duration
          },
          {:video_parser, video_track.id} => %Membrane.H264.FFmpeg.Parser{
            alignment: :au,
            attach_nalus?: true,
            framerate: state.framerate
          }
        },
        links:
          link ++
            [
              link({:track_sync, track.stream_id})
              |> via_out(Pad.ref(:output, :audio))
              |> then(link_to_opus_aac_transcoder)
              |> via_in(Pad.ref(:input, {:audio, audio_track.id}), options: [encoding: :AAC])
              |> to({:hls_sink_bin, track.stream_id}),
              link({:track_sync, track.stream_id})
              |> via_out(Pad.ref(:output, :video))
              |> to({:keyframe_requester, video_track.id})
              |> to({:video_parser, video_track.id})
              |> then(link_to_transcoder)
              |> via_in(Pad.ref(:input, {:video, video_track.id}), options: [encoding: :H264])
              |> to({:hls_sink_bin, track.stream_id})
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

    defp maybe_create_opus_aac_transcoder_link(audio_track) do
      opus_aac_transcoding =
        &(to(&1, {:opus_decoder, audio_track.id}, Membrane.Opus.Decoder)
          |> to({:aac_encoder, audio_track.id}, Membrane.AAC.FDK.Encoder)
          |> to({:aac_parser, audio_track.id}, %Membrane.AAC.Parser{
            out_encapsulation: :none
          }))

      if audio_track.encoding == :OPUS,
        do: opus_aac_transcoding,
        else: & &1
    end

    if Enum.all?(@opus_deps, &Code.ensure_loaded?/1) do
      defp hls_links_and_children(
             link_builder,
             :OPUS,
             track,
             _segment_duration,
             _framerate,
             _transcoding_config
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
    else
      defp hls_links_and_children(
             _link_builder,
             :OPUS,
             _track,
             _segment_duration,
             _framerate,
             _transcoding_config
           ) do
        raise """
        Cannot find one of the modules required to support Opus audio input.
        Ensure `:membrane_opus_plugin`, `:membrane_aac_plugin` and `:membrane_aac_fdk_plugin` are added to the deps.
        """
      end
    end

    defp hls_links_and_children(
           link_builder,
           :AAC,
           track,
           _segment_duration,
           _framerate,
           _transcoding_config
         ),
         do: %ParentSpec{
           children: %{},
           links: [
             link_builder
             |> via_in(Pad.ref(:input, {:audio, track.id}), options: [encoding: :AAC])
             |> to({:hls_sink_bin, track.stream_id})
           ]
         }

    defp hls_links_and_children(
           link_builder,
           :H264,
           track,
           segment_duration,
           framerate,
           transcoding_config
         ) do
      link_to_transcoder = create_transcoder_link(transcoding_config, track.id)

      %ParentSpec{
        children: %{
          {:keyframe_requester, track.id} => %Membrane.KeyframeRequester{
            interval: segment_duration
          },
          {:video_parser, track.id} => %Membrane.H264.FFmpeg.Parser{
            alignment: :au,
            attach_nalus?: true,
            framerate: framerate
          }
        },
        links: [
          link_builder
          |> to({:keyframe_requester, track.id})
          |> to({:video_parser, track.id})
          |> then(link_to_transcoder)
          |> via_in(Pad.ref(:input, {:video, track.id}), options: [encoding: :H264])
          |> to({:hls_sink_bin, track.stream_id})
        ]
      }
    end

    defp create_transcoder_link(%TranscodingConfig{enabled?: false}, _track_id), do: & &1

    if Enum.all?(@transcoding_deps, &Code.ensure_loaded?/1) do
      defp create_transcoder_link(transcoding_config, track_id) do
        resolution_scaler = %Membrane.FFmpeg.SWScale.Scaler{
          output_width: transcoding_config.output_width,
          output_height: transcoding_config.output_height
        }

        framerate_converter = %Membrane.FramerateConverter{
          framerate: transcoding_config.output_framerate
        }

        video_parser_out = %Membrane.H264.FFmpeg.Parser{
          alignment: :au,
          attach_nalus?: true,
          framerate: transcoding_config.output_framerate
        }

        fn link_builder ->
          link_builder
          |> to({:decoder, track_id}, Membrane.H264.FFmpeg.Decoder)
          |> to({:resolution_scaler, track_id}, resolution_scaler)
          |> to({:framerate_converter, track_id}, framerate_converter)
          |> to({:encoder, track_id}, %Membrane.H264.FFmpeg.Encoder{profile: :baseline})
          |> to({:video_parser_out, track_id}, video_parser_out)
        end
      end
    else
      defp create_transcoder_link(_transcoding_config, _track_id) do
        raise """
        Cannot find some of the modules required to perform transcoding.
        Ensure `:membrane_ffmpeg_swscale_plugin` and `membrane_framerate_converter_plugin` are added to the deps.
        """
      end
    end
  end
end
