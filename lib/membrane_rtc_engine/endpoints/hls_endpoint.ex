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

    @opus_deps [Membrane.Opus.Decoder, Membrane.AAC.Parser, Membrane.AAC.FDK.Encoder]
    @transcoding_deps [
      Membrane.H264.FFmpeg.Decoder,
      Membrane.H264.FFmpeg.Encoder,
      Membrane.FFmpeg.SWScale.Scaler,
      Membrane.FramerateConverter,
      Membrane.VideoMixer
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
        framerate: opts.framerate,
        target_segment_duration: opts.target_segment_duration,
        transcoding_config: opts.transcoding_config,
        event_id: UUID.uuid4()
      }

      {:ok, state}
    end

    @impl true
    def handle_other({:new_tracks, tracks}, ctx, state) do
      {:endpoint, endpoint_id} = ctx.name
      # TODO delete "track.type == :video" when audio mixer is added
      tracks = Enum.filter(tracks, fn track -> :raw in track.format and track.type == :video end)

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
      # notify about playable just when video becomes available
      send(state.owner, {:playlist_playable, content_type, state.event_id})
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
      children_to_remove =
        [
          :opus_decoder,
          :aac_encoder,
          :aac_parser,
          :keyframe_requester,
          :video_parser,
          :decoder,
          :framerate_converter
        ]
        |> Enum.map(&{&1, track_id})
        |> Enum.filter(&Map.has_key?(ctx.children, &1))

      {_removed_track, tracks} = Map.pop!(state.tracks, track_id)
      state = %{state | tracks: tracks}

      any_video_track_left? =
        Enum.any?(tracks, fn {_id, track} ->
          track.type == :video
        end)

      children_to_remove =
        if any_video_track_left? do
          children_to_remove
        else
          add_video_sink_children(children_to_remove)
        end

      {{:ok, remove_child: children_to_remove}, state}
    end

    defp add_video_sink_children(children) do
      sink_children = [:compositor, :encoder, :video_parser_out, :hls_sink_bin]
      children ++ sink_children
    end

    @impl true
    def handle_pad_added(Pad.ref(:input, track_id) = pad, ctx, state) do
      link_builder = link_bin_input(pad)
      track = Map.get(state.tracks, track_id)

      directory = Path.join(state.output_directory, state.event_id)

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
        if Map.has_key?(ctx.children, :hls_sink_bin) do
          {spec, state}
        else
          # remove directory if it already exists
          File.rm_rf(directory)
          File.mkdir_p!(directory)

          ffmpeg_filters = fn
            width, height, 1 ->
              "[0:v]scale=#{width}:#{height}:force_original_aspect_ratio=decrease,setsar=1/1,pad=#{width}:#{height}:(ow-iw)/2:(oh-ih)/2"

            width, height, 2 ->
              "[0:v]scale=-1:#{height}:force_original_aspect_ratio=decrease,crop=#{width}/2:ih,setsar=1[left];"
              |> Kernel.<>(
                "[1:v]scale=-1:#{height}:force_original_aspect_ratio=decrease,crop=#{width}/2:ih,setsar=1[right];"
              )
              |> Kernel.<>("[left][right]hstack")

            width, height, 3 ->
              "[0:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{width}/2:ih,setsar=1[lefttop];"
              |> Kernel.<>(
                "[1:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{width}/2:ih,setsar=1[leftdown];"
              )
              |> Kernel.<>(
                "[2:v]scale=-1:#{height}:force_original_aspect_ratio=decrease,crop=#{width}/2:ih,setsar=1[right];"
              )
              |> Kernel.<>("[lefttop][leftdown]vstack=inputs=2[left];")
              |> Kernel.<>("[left][right]hstack=inputs=2")

            width, height, 4 ->
              "[0:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{width}/2:ih,setsar=1[lefttop];"
              |> Kernel.<>(
                "[1:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{width}/2:ih,setsar=1[righttop];"
              )
              |> Kernel.<>(
                "[2:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{width}/2:ih,setsar=1[leftdown];"
              )
              |> Kernel.<>(
                "[3:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{width}/2:ih,setsar=1[rightdown];"
              )
              |> Kernel.<>("[lefttop][righttop]hstack=inputs=2[top];")
              |> Kernel.<>("[leftdown][rightdown]hstack=inputs=2[down];")
              |> Kernel.<>("[top][down]vstack=inputs=2")

            width, height, 5 ->
              rest = rem(width, 3)
              column_width = div(width, 3)

              "[0:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{column_width}:ih,setsar=1[lefttop];"
              |> Kernel.<>(
                "[1:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{column_width}:ih,setsar=1[middletop];"
              )
              |> Kernel.<>(
                "[2:v]scale=-1:#{height}:force_original_aspect_ratio=decrease,crop=#{column_width}+#{rest}:ih,setsar=1[right];"
              )
              |> Kernel.<>(
                "[3:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{column_width}:ih,setsar=1[leftdown];"
              )
              |> Kernel.<>(
                "[4:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{column_width}:ih,setsar=1[middledown];"
              )
              |> Kernel.<>(
                "[lefttop][middletop]hstack=inputs=2[top];[leftdown][middledown]hstack=inputs=2[bottom];"
              )
              |> Kernel.<>("[top][bottom]vstack=inputs=2[left];")
              |> Kernel.<>("[left][right]hstack=inputs=2")

            width, height, 6 ->
              rest = rem(width, 3)
              column_width = div(width, 3)

              "[0:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{column_width}:ih,setsar=1[lefttop];"
              |> Kernel.<>(
                "[1:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{column_width}:ih,setsar=1[middletop];"
              )
              |> Kernel.<>(
                "[2:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{column_width}+#{rest}:ih,setsar=1[righttop];"
              )
              |> Kernel.<>(
                "[3:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{column_width}:ih,setsar=1[leftdown];"
              )
              |> Kernel.<>(
                "[4:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{column_width}:ih,setsar=1[middledown];"
              )
              |> Kernel.<>(
                "[5:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{column_width}+#{rest}:ih,setsar=1[rightdown];"
              )
              |> Kernel.<>("[lefttop][middletop][righttop]hstack=inputs=3[top];")
              |> Kernel.<>("[leftdown][middledown][rightdown]hstack=inputs=3[bottom];")
              |> Kernel.<>("[top][bottom]vstack=inputs=2")

            _, _, n ->
              raise("No matching filter found for #{n} input(s)")
          end

          compositor = %Membrane.VideoMixer{
            output_caps: %Membrane.RawVideo{
              width: state.transcoding_config.output_width,
              height: state.transcoding_config.output_height,
              pixel_format: :I420,
              framerate: state.transcoding_config.output_framerate,
              aligned: true
            },
            filters: ffmpeg_filters
          }

          video_parser_out = %Membrane.H264.FFmpeg.Parser{
            alignment: :au,
            attach_nalus?: true,
            framerate: state.transcoding_config.output_framerate
          }

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

          hls_sink_spec = %ParentSpec{
            children: %{
              :compositor => compositor,
              :video_parser_out => video_parser_out,
              :hls_sink_bin => hls_sink_bin
            },
            links: [
              link(:compositor)
              |> to(:encoder, %Membrane.H264.FFmpeg.Encoder{
                profile: :baseline
              })
              |> to(:video_parser_out)
              |> via_in(Pad.ref(:input, :video), options: [encoding: :H264])
              |> to(:hls_sink_bin)
            ]
          }

          {merge_parent_specs(spec, hls_sink_spec), state}
        end

      {{:ok, spec: spec}, state}
    end

    defp merge_parent_specs(spec1, spec2) do
      %ParentSpec{
        children: Map.merge(spec1.children, spec2.children),
        links: spec1.links ++ spec2.links
      }
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
           _framerate,
           transcoding_config
         ) do
      transcoding_interceptor = create_transcoder_link(transcoding_config, track.id)

      %ParentSpec{
        children: %{
          {:keyframe_requester, track.id} => %Membrane.KeyframeRequester{
            interval: segment_duration
          },
          {:video_parser, track.id} => %Membrane.H264.FFmpeg.Parser{
            alignment: :au,
            attach_nalus?: true
          }
        },
        links: [
          link_builder
          |> to({:keyframe_requester, track.id})
          |> to({:video_parser, track.id})
          |> then(transcoding_interceptor)
        ]
      }
    end

    defp create_transcoder_link(%TranscodingConfig{enabled?: false}, _track_id), do: & &1

    if Enum.all?(@transcoding_deps, &Code.ensure_loaded?/1) do
      defp create_transcoder_link(transcoding_config, track_id) do
        framerate_converter = %Membrane.FramerateConverter{
          framerate: transcoding_config.output_framerate
        }

        fn link_builder ->
          link_builder
          |> to({:decoder, track_id}, Membrane.H264.FFmpeg.Decoder)
          |> to({:framerate_converter, track_id}, framerate_converter)
          |> to(:compositor)
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
