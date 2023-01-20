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

    alias Membrane.RTC.Engine
    alias Membrane.RTC.Engine.Endpoint.HLS.TranscodingConfig
    alias Membrane.RTC.Engine.Endpoint.WebRTC.TrackReceiver
    alias Membrane.RTC.Engine.Track

    @transcoding_deps [
      Membrane.H264.FFmpeg.Decoder,
      Membrane.H264.FFmpeg.Encoder,
      Membrane.FFmpeg.SWScale.Scaler,
      Membrane.FramerateConverter
    ]

    def_input_pad :input,
      demand_unit: :buffers,
      accepted_format: _any,
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
    def handle_init(_ctx, opts) do
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

      {[], state}
    end

    @impl true
    def handle_parent_notification({:new_tracks, tracks}, ctx, state) do
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

      {[], state}
    end

    @impl true
    def handle_parent_notification(msg, _ctx, state) do
      Membrane.Logger.warn("Unexpected message: #{inspect(msg)}. Ignoring.")
      {[], state}
    end

    def handle_child_notification(
          {:track_playable, {content_type, track_id}},
          {:hls_sink_bin, stream_id},
          _ctx,
          state
        ) do
      %{origin: origin} = Map.fetch!(state.tracks, track_id)
      # notify about playable just when video becomes available
      send(state.owner, {:playlist_playable, content_type, stream_id, origin})
      {[], state}
    end

    @impl true
    def handle_child_notification(notification, _element, _context, state) do
      Membrane.Logger.warn("Unexpected notification: #{inspect(notification)}. Ignoring.")
      {[], state}
    end

    @impl true
    def handle_pad_removed(Pad.ref(:input, track_id), ctx, state) do
      children =
        [
          :opus_decoder,
          :aac_encoder,
          :aac_parser,
          :video_parser,
          :video_parser_out,
          :decoder,
          :encoder,
          :resolution_scaler,
          :track_receiver,
          :depayloader,
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

      {[remove_child: children], state}
    end

    @impl true
    def handle_pad_added(Pad.ref(:input, track_id) = pad, _ctx, state) do
      link_builder = bin_input(pad)
      track = Map.get(state.tracks, track_id)

      directory = Path.join(state.output_directory, track.stream_id)

      structure = hls_structure(link_builder, track, state)

      state =
        if MapSet.member?(state.stream_ids, track.stream_id) do
          state
        else
          # remove directory if it already exists
          File.rm_rf(directory)
          File.mkdir_p!(directory)

          %{state | stream_ids: MapSet.put(state.stream_ids, track.stream_id)}
        end

      {[spec: {structure, []}], state}
    end

    defp hls_structure(link_builder, %Track{encoding: :OPUS} = track, state) do
      [
        link_builder
        |> child({:track_receiver, track.id}, %TrackReceiver{
          track: track,
          initial_target_variant: :high
        })
        |> child({:depayloader, track.id}, get_depayloader(track))
        |> child({:opus_decoder, track.id}, Membrane.Opus.Decoder)
        |> child({:aac_encoder, track.id}, Membrane.AAC.FDK.Encoder)
        |> child({:aac_parser, track.id}, %Membrane.AAC.Parser{out_encapsulation: :none})
        |> via_in(Pad.ref(:input, {:audio, track.id}),
          options: [
            encoding: :AAC,
            segment_duration: %Membrane.HTTPAdaptiveStream.Sink.SegmentDuration{
              min: state.target_segment_duration - Membrane.Time.seconds(1),
              target: state.target_segment_duration
            }
          ]
        )
        |> child({:hls_sink_bin, track.stream_id}, hls_sink_bin(track, state), get_if_exists: true)
      ]
    end

    defp hls_structure(link_builder, %Track{encoding: :H264} = track, state) do
      link_to_transcoder = create_transcoder_link(state.transcoding_config, track.id)

      [
        link_builder
        |> child({:track_receiver, track.id}, %TrackReceiver{
          track: track,
          initial_target_variant: :high,
          keyframe_request_interval: state.target_segment_duration
        })
        |> child({:depayloader, track.id}, get_depayloader(track))
        |> child({:video_parser, track.id}, %Membrane.H264.FFmpeg.Parser{
          alignment: :au,
          attach_nalus?: true,
          framerate: state.framerate
        })
        |> then(link_to_transcoder)
        |> via_in(Pad.ref(:input, {:video, track.id}),
          options: [
            encoding: :H264,
            segment_duration: %Membrane.HTTPAdaptiveStream.Sink.SegmentDuration{
              min: state.target_segment_duration - Membrane.Time.seconds(1),
              target: state.target_segment_duration
            }
          ]
        )
        |> child({:hls_sink_bin, track.stream_id}, hls_sink_bin(track, state), get_if_exists: true)
      ]
    end

    defp hls_sink_bin(track, state),
      do: %Membrane.HTTPAdaptiveStream.SinkBin{
        manifest_module: Membrane.HTTPAdaptiveStream.HLS,
        target_window_duration: state.target_window_duration,
        persist?: false,
        storage: %Membrane.HTTPAdaptiveStream.Storages.FileStorage{
          directory: Path.join(state.output_directory, track.stream_id)
        },
        hls_mode: state.hls_mode
      }

    defp get_depayloader(track) do
      track
      |> Track.get_depayloader()
      |> tap(&unless &1, do: raise("Couldn't find depayloader for track #{inspect(track)}"))
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
          |> child({:decoder, track_id}, Membrane.H264.FFmpeg.Decoder)
          |> child({:resolution_scaler, track_id}, resolution_scaler)
          |> child({:framerate_converter, track_id}, framerate_converter)
          |> child({:encoder, track_id}, Membrane.H264.FFmpeg.Encoder)
          |> child({:video_parser_out, track_id}, video_parser_out)
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
