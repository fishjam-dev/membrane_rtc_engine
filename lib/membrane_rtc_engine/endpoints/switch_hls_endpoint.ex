if Enum.all?(
     [
       Membrane.H264.FFmpeg.Parser,
       Membrane.HTTPAdaptiveStream.SinkBin
     ],
     &Code.ensure_loaded?/1
   ) do
  defmodule Membrane.RTC.Engine.Endpoint.SwitchHLS do
    @moduledoc """
    An Endpoint responsible for converting incoming tracks to HLS playlist.

    This module requires the following plugins to be present in your `mix.exs` for H264 & AAC input:
    ```
    [
      :membrane_h264_ffmpeg_plugin,
      :membrane_http_adaptive_stream_plugin,
    ]
    ```

    It can perform transcoding, in such case these plugins are also needed:
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
    alias Membrane.RTC.Engine.Endpoint.HLS.Utils
    alias Membrane.RTC.Engine.Endpoint.HLS.TranscodingConfig

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
                room_id: [
                  spec: String.t(),
                  description: "Identificator of the rtc_engine instance used in output directory name",
                  default: ""
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
        output_directory: Path.join(opts.output_directory, opts.room_id),
        owner: opts.owner,
        hls_mode: opts.hls_mode,
        target_window_duration: opts.target_window_duration,
        framerate: opts.framerate,
        target_segment_duration: opts.target_segment_duration,
        linked: %{video: false, audio: false},
        room_id: opts.room_id,
        transcoding_config: opts.transcoding_config
      }

      File.mkdir_p!(state.output_directory)

      {:ok, state}
    end

    @impl true
    def handle_prepared_to_playing(_ctx, state) do
      children =
        [
          hls_sink_bin: %Membrane.HTTPAdaptiveStream.SinkBin{
            manifest_module: Membrane.HTTPAdaptiveStream.HLS,
            target_window_duration: state.target_window_duration,
            target_segment_duration: state.target_segment_duration,
            persist?: false,
            storage: %Membrane.HTTPAdaptiveStream.Storages.FileStorage{
              directory: state.output_directory
            },
            hls_mode: state.hls_mode
          }
        ]

      {{:ok, spec: %ParentSpec{children: children}}, state}
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
          :hls_sink_bin,
          _ctx,
          state
        ) do
      # TODO tu czasami jest race con, kiedy przestane byc prezenterem zanim dostane playlist playable
      %{origin: origin} = Map.fetch!(state.tracks, track_id)
      # notify about playable just when video becomes available
      send(state.owner, {:playlist_playable, content_type, state.room_id, origin})
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
    def handle_pad_removed(Pad.ref(:input, track_id), _ctx, state) do
      state = %{state | tracks: Map.delete(state.tracks, track_id)}
      {:ok, state}
    end

    @impl true
    def handle_pad_added(Pad.ref(:input, track_id) = pad, _ctx, state) do
      track = Map.get(state.tracks, track_id)
      track_type = Utils.get_track_type(track)

      {spec, state} = if not state.linked[track_type] do
        children = %{
          {:switch, track_type} => %Membrane.RTC.Engine.Endpoint.HLS.Switch{type: track_type},
          {:generator_realtimer, track_type} => Membrane.Realtimer,
          {:generator_metadata_fixer, track_type} => Membrane.RTC.Engine.Endpoint.HLS.MetadataFixer
        }

        audio_caps = %Membrane.RawAudio{
          channels: 1,
          sample_rate: 16_000,
          sample_format: :s16le
        }

        video_caps = %Membrane.RawVideo{
          pixel_format: :I420,
          height: 720,
          width: 1280,
          framerate: {24, 1},
          aligned: true
        }

        children = case track_type do
          :video ->
            children
            |> Map.put({:generator, track_type}, %Membrane.BlankVideoGenerator{
              caps: video_caps,
              duration: :infinity
            })
            |> Map.put({:generator_encoder, track_type}, Membrane.H264.FFmpeg.Encoder)
            |> Map.put({:generator_parser, track_type}, %Membrane.H264.FFmpeg.Parser{
              framerate: video_caps.framerate
            })
          :audio ->
            {audio_encoder, audio_parser} = if track.encoding == :AAC do
              {Membrane.AAC.FDK.Encoder, Membrane.AAC.Parser}
            else
              {%Membrane.Opus.Encoder{
                input_caps: audio_caps
              }, Membrane.Opus.Parser}
            end
            children
            |> Map.put({:generator, track_type}, %Membrane.SilenceGenerator{
              caps: audio_caps,
              duration: :infinity
            })
            |> Map.put({:generator_encoder, track_type}, audio_encoder)
            |> Map.put({:generator_parser, track_type}, audio_parser)
        end

        links = [
          link({:generator, track_type})
          |> to({:generator_encoder, track_type})
          |> to({:generator_parser, track_type})
          |> to({:generator_metadata_fixer, track_type})
          |> to({:generator_realtimer, track_type})
          |> via_in(Pad.ref(:input, :static), options: [track: nil])
          |> to({:switch, track_type})
        ]

        spec = hls_links_and_children(
          link({:switch, track_type}),
          track.encoding,
          track,
          state.target_segment_duration,
          state.framerate,
          state.transcoding_config
        )

        spec = %ParentSpec{spec | children: Map.merge(spec.children, children)}
        spec = %ParentSpec{spec | links: spec.links ++ links}
        state = put_in(state, [:linked, track_type], true)

        {spec, state}
      else
        {%ParentSpec{}, state}
      end

      switch_link =
        link_bin_input(pad)
        |> via_in(Pad.ref(:input, track.id), options: [track: track])
        |> to({:switch, track_type})

      spec = %ParentSpec{
        children: spec.children,
        links: [ switch_link | spec.links]
      }

      # naive implementation - the last pad linked is selected by the switch
      create_message = fn track_type -> if state.linked[track_type], do: [{{:switch, track_type}, {:change_origin, track.origin}}], else: [] end
      switch_message = create_message.(:audio) ++ create_message.(:video)

      {{:ok, spec: spec, forward: switch_message}, state}
    end

    if Enum.all?(@opus_deps, &Code.ensure_loaded?/1) do
      defp hls_links_and_children(link_builder, :OPUS, track, _segment_duration, _framerate, _transcoding_config) do
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
            |> to(:hls_sink_bin)
          ]
        }
      end
    else
      defp hls_links_and_children(_link_builder, :OPUS, _track, _segment_duration, _framerate, _transcoding_config) do
        raise """
        Cannot find one of the modules required to support Opus audio input.
        Ensure `:membrane_opus_plugin`, `:membrane_aac_plugin` and `:membrane_aac_fdk_plugin` are added to the deps.
        """
      end
    end

    defp hls_links_and_children(link_builder, :AAC, track, _segment_duration, _framerate, _transcoding_config),
      do: %ParentSpec{
        children: %{},
        links: [
          link_builder
          |> via_in(Pad.ref(:input, {:audio, track.id}), options: [encoding: :AAC])
          |> to(:hls_sink_bin)
        ]
      }

    defp hls_links_and_children(link_builder, :H264, track, segment_duration, framerate, transcoding_config) do
      transcoding_interceptor = create_transcoding_interceptor(transcoding_config, track.id)

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
          |> then(transcoding_interceptor)
          |> via_in(Pad.ref(:input, {:video, track.id}), options: [encoding: :H264])
          |> to(:hls_sink_bin)
        ]
      }
    end

    defp create_transcoding_interceptor(%TranscodingConfig{enabled?: false}, _track_id), do: & &1

    if Enum.all?(@transcoding_deps, &Code.ensure_loaded?/1) do
      defp create_transcoding_interceptor(transcoding_config, track_id) do
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
          |> to({:encoder, track_id}, Membrane.H264.FFmpeg.Encoder)
          |> to({:video_parser_out, track_id}, video_parser_out)
        end
      end
    else
      defp create_transcoding_interceptor(_transcoding_config, _track_id) do
        raise """
        Cannot find some of the modules required to perform transcoding.
        Ensure `:membrane_ffmpeg_swscale_plugin` and `membrane_framerate_converter_plugin` are added to the deps.
        """
      end
    end
  end
end
