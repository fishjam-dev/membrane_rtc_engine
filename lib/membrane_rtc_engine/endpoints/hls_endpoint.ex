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

    alias Membrane.RTC.Engine.Endpoint.HLS.{
      AudioMixerConfig,
      StreamFormatUpdater,
      CompositorConfig,
      SinkBinConfig
    }

    alias Membrane.RTC.Engine.Endpoint.WebRTC.TrackReceiver
    alias Membrane.RTC.Engine.Track
    alias Membrane.Time
    alias Membrane.VideoCompositor.RustStructs.BaseVideoPlacement

    @compositor_deps [
      Membrane.H264.FFmpeg.Decoder,
      Membrane.H264.FFmpeg.Encoder,
      Membrane.FFmpeg.SWScale.Scaler,
      Membrane.FramerateConverter,
      Membrane.BlankVideoGenerator,
      Membrane.Realtimer
    ]
    @audio_mixer_deps [
      Membrane.AudioMixer,
      Membrane.AAC.Parser,
      Membrane.AAC.FDK.Encoder
    ]

    @initial_placement %BaseVideoPlacement{
      position: {0, 0},
      size: {100, 100},
      z_value: 0.0
    }

    @track_children [
      :opus_decoder,
      :aac_encoder,
      :aac_parser,
      :video_parser,
      :decoder,
      :framerate_converter,
      :track_receiver,
      :depayloader,
      :blank,
      :realtimer,
      :audio_filler,
      :stream_format_updater
    ]

    def_input_pad :input,
      demand_unit: :buffers,
      accepted_format: _any,
      availability: :on_request

    def_options rtc_engine: [
                  spec: pid(),
                  description: "Pid of parent Engine"
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
                output_directory: [
                  spec: Path.t(),
                  description: "Path to directory under which HLS output will be saved",
                  default: "hls_output"
                ],
                mixer_config: [
                  spec: %{audio: AudioMixerConfig.t(), video: CompositorConfig.t()} | nil,
                  default: nil,
                  description: """
                  Audio and video mixer configuration. If you don't want to use compositor pass nil.
                  """
                ],
                sink_bin_config: [
                  spec: SinkBinConfig.t(),
                  default: SinkBinConfig,
                  description: """
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
                partial_segment_duration: [
                  spec: SegmentDuration.t() | nil,
                  default: nil,
                  description: """
                  Expected length of each partial segment. Setting it is not necessary, but
                  may help players achieve better UX.
                  """
                ]

    @impl true
    def handle_init(_context, options) do
      state =
        options
        |> Map.from_struct()
        |> Map.merge(%{
          tracks: %{},
          stream_beginning: nil,
          video_layout_state: nil,
          stream_ids: MapSet.new(),
          video_layout_tracks_added: %{}
        })

      video_layout_state =
        if is_nil(options.mixer_config),
          do: nil,
          else:
            state.mixer_config.video.layout_module.init(options.mixer_config.video.stream_format)

      {[], %{state | video_layout_state: video_layout_state}}
    end

    @impl true
    def handle_playing(_context, %{mixer_config: nil} = state), do: {[], state}

    @impl true
    def handle_playing(context, state) do
      spec =
        generate_audio_mixer(state, context) ++
          generate_compositor(state, context) ++
          get_hls_sink_spec(state, %{stream_id: nil}, state.output_directory)

      {[spec: spec], state}
    end

    @impl true
    def handle_pad_removed(Pad.ref(:input, track_id), ctx, state) do
      track_children =
        @track_children
        |> Enum.map(&{&1, track_id})
        |> Enum.filter(&Map.has_key?(ctx.children, &1))

      {removed_track, tracks} = Map.pop!(state.tracks, track_id)
      state = %{state | tracks: tracks}

      sink_bin_used? =
        Enum.any?(tracks, fn {_id, track} ->
          track.stream_id == removed_track.stream_id
        end)

      {state, children_to_remove} =
        if is_nil(state.mixer_config) and not sink_bin_used?,
          do: {state, [{:hls_sink_bin, removed_track.stream_id}]},
          else: {state, []}

      children_to_remove = track_children ++ children_to_remove

      {update_layout_actions, state} =
        compositor_update_layout(:remove, removed_track, nil, state)

      result_actions = [remove_child: children_to_remove] ++ update_layout_actions

      state = %{
        state
        | video_layout_tracks_added: Map.delete(state.video_layout_tracks_added, track_id)
      }

      {result_actions, state}
    end

    @impl true
    def handle_pad_added(Pad.ref(:input, track_id) = pad, ctx, state) do
      {offset, state} = get_track_offset(state)
      track = Map.get(state.tracks, track_id)
      directory = get_hls_stream_directory(state, track)

      track_spec = get_track_spec(offset, bin_input(pad), track, state, ctx)

      {spec, state} =
        if hls_sink_bin_exists?(track, ctx, state) do
          {track_spec, state}
        else
          hls_sink_spec = get_hls_sink_spec(state, track, directory)
          {track_spec ++ hls_sink_spec, state}
        end

      {[spec: spec], state}
    end

    @impl true
    def handle_child_notification(
          {:update_layout, stream_format},
          {:stream_format_updater, track_id} = child,
          _ctx,
          state
        ) do
      track = Map.get(state.tracks, track_id)

      action = if is_map_key(state.video_layout_tracks_added, track_id), do: :update, else: :add

      {update_layout_action, state} =
        compositor_update_layout(action, track, stream_format, state)

      result_actions = update_layout_action ++ [notify_child: {child, :layout_updated}]
      {_poped_value, state} = pop_in(state, [:video_layout_tracks_added, track_id])

      {result_actions, state}
    end

    def handle_child_notification(
          {:track_playable, content_type},
          :hls_sink_bin,
          _ctx,
          state
        ) do
      send(state.owner, {:playlist_playable, content_type, ""})
      {[], state}
    end

    def handle_child_notification(
          {:track_playable, {content_type, _track_id}},
          {:hls_sink_bin, stream_id},
          _ctx,
          state
        ) do
      send(state.owner, {:playlist_playable, content_type, stream_id})
      {[], state}
    end

    @impl true
    def handle_child_notification(notification, _element, _context, state) do
      Membrane.Logger.warn("Unexpected notification: #{inspect(notification)}. Ignoring.")
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

    defp get_hls_sink_spec(state, track, directory) do
      File.rm_rf(directory)
      File.mkdir_p!(directory)

      config = %{
        state.sink_bin_config
        | storage: state.sink_bin_config.storage.(directory),
          mp4_parameters_in_band?: is_nil(state.mixer_config)
      }

      hls_sink = struct!(Membrane.HTTPAdaptiveStream.SinkBin, Map.from_struct(config))

      child_name =
        if is_nil(state.mixer_config), do: {:hls_sink_bin, track.stream_id}, else: :hls_sink_bin

      [child(child_name, hls_sink)]
    end

    defp get_track_spec(
           offset,
           link_builder,
           track,
           state,
           ctx
         ) do
      get_depayloading_track_spec(link_builder, track) ++
        attach_track_spec(offset, track, state) ++
        generate_blank_spec(state, ctx) ++
        generate_silence_spec(state, ctx)
    end

    defp get_depayloading_track_spec(link_builder, track),
      do: [
        link_builder
        |> child({:track_receiver, track.id}, %TrackReceiver{
          track: track,
          initial_target_variant: :high
        })
        |> child({:depayloader, track.id}, get_depayloader(track))
      ]

    defp attach_track_spec(offset, %{type: :audio} = track, state),
      do: attach_audio_track_spec(offset, track, state)

    defp attach_track_spec(offset, %{type: :video} = track, state),
      do: attach_video_track_spec(offset, track, state)

    defp attach_audio_track_spec(_offset, track, %{mixer_config: nil} = state),
      do: [
        get_child({:depayloader, track.id})
        |> child({:opus_decoder, track.id}, Membrane.AAC.FDK.Encoder)
        |> child({:aac_encoder, track.id}, %Membrane.AAC.Parser{out_encapsulation: :none})
        |> child({:aac_parser, track.id}, Membrane.Opus.Decoder)
        |> via_in(Pad.ref(:input, {:audio, track.id}),
          options: [
            encoding: :AAC,
            segment_duration: state.segment_duration,
            partial_segment_duration: state.partial_segment_duration
          ]
        )
        |> get_child({:hls_sink_bin, track.stream_id})
      ]

    defp attach_audio_track_spec(offset, track, _state),
      do: [
        get_child({:depayloader, track.id})
        |> child({:opus_decoder, track.id}, Membrane.Opus.Decoder)
        |> child({:audio_filler, track.id}, Membrane.AudioFiller)
        |> via_in(Pad.ref(:input, {:extra, track.id}), options: [offset: offset])
        |> get_child(:audio_mixer)
      ]

    defp attach_video_track_spec(_offset, track, %{mixer_config: nil} = state),
      do: [
        get_child({:depayloader, track.id})
        |> child({:video_parser, track.id}, %Membrane.H264.FFmpeg.Parser{
          alignment: :au,
          attach_nalus?: true
        })
        |> via_in(Pad.ref(:input, {:video, track.id}),
          options: [
            encoding: :H264,
            segment_duration: state.segment_duration,
            partial_segment_duration: state.partial_segment_duration
          ]
        )
        |> get_child({:hls_sink_bin, track.stream_id})
      ]

    defp attach_video_track_spec(offset, track, _state),
      do: [
        get_child({:depayloader, track.id})
        |> child({:video_parser, track.id}, %Membrane.H264.FFmpeg.Parser{
          attach_nalus?: true,
          alignment: :au
        })
        |> child({:stream_format_updater, track.id}, StreamFormatUpdater)
        |> child({:decoder, track.id}, Membrane.H264.FFmpeg.Decoder)
        |> via_in(Pad.ref(:input, track.id),
          options: [initial_placement: @initial_placement, timestamp_offset: offset]
        )
        |> get_child(:compositor)
      ]

    if Enum.all?(@compositor_deps, &Code.ensure_loaded?/1) do
      defp generate_silence_spec(%{mixer_config: nil}, _ctx), do: []

      defp generate_silence_spec(_state, ctx) when is_map_key(ctx.children, :silence_generator),
        do: []

      defp generate_silence_spec(state, _ctx) do
        silence_generator = %Membrane.SilenceGenerator{
          stream_format: %Membrane.RawAudio{
            channels: state.mixer_config.audio.channels,
            sample_rate: state.mixer_config.audio.sample_rate,
            sample_format: state.mixer_config.audio.sample_format
          },
          duration: :infinity,
          frames_per_buffer: 960
        }

        [
          child(:silence_generator, silence_generator)
          |> child(:audio_realtimer, Membrane.Realtimer)
          |> get_child(:audio_mixer)
        ]
      end

      defp generate_blank_spec(%{mixer_config: nil}, _ctx), do: []

      defp generate_blank_spec(_state, ctx) when is_map_key(ctx.children, :fake_source), do: []

      defp generate_blank_spec(state, _ctx) do
        stream_format = state.mixer_config.video.stream_format

        generator_stream_format = %Membrane.RawVideo{
          width: stream_format.width,
          height: stream_format.height,
          pixel_format: :I420,
          framerate: stream_format.framerate,
          aligned: true
        }

        [
          child(:fake_source, %Membrane.BlankVideoGenerator{
            stream_format: generator_stream_format,
            duration: :infinity
          })
          |> child(:video_realtimer, Membrane.Realtimer)
          |> via_in(:input, options: [initial_placement: @initial_placement])
          |> get_child(:compositor)
        ]
      end

      defp generate_compositor(_state, ctx) when is_map_key(ctx.children, :compositor), do: []

      defp generate_compositor(state, _ctx) do
        compositor = %Membrane.VideoCompositor{
          stream_format: state.mixer_config.video.stream_format,
          real_time: false
        }

        video_parser_out = %Membrane.H264.FFmpeg.Parser{
          alignment: :au,
          attach_nalus?: true
        }

        {frames_per_second, 1} = state.mixer_config.video.stream_format.framerate
        seconds_number = Membrane.Time.as_seconds(state.segment_duration.target)

        [
          child(:compositor, compositor)
          |> child(:encoder, %Membrane.H264.FFmpeg.Encoder{
            profile: :baseline,
            gop_size: frames_per_second * seconds_number
          })
          |> child(:video_parser_out, video_parser_out)
          |> via_in(Pad.ref(:input, :video),
            options: [
              encoding: :H264,
              segment_duration: state.segment_duration,
              partial_segment_duration: state.partial_segment_duration
            ]
          )
          |> get_child(:hls_sink_bin)
        ]
      end
    else
      defp generate_compositor(_state, _ctx) do
        raise """
        Cannot find some of the modules required to use the video composer.
        Ensure that the following dependencies are added to the deps.
        #{merge_strings(@compositor_deps)}
        """
      end
    end

    if Enum.all?(@audio_mixer_deps, &Code.ensure_loaded?/1) do
      defp generate_audio_mixer(_state, ctx) when is_map_key(ctx.children, :audio_mixer), do: []

      defp generate_audio_mixer(state, _ctx) do
        audio_mixer = %Membrane.AudioMixer{
          stream_format: %Membrane.RawAudio{
            channels: state.mixer_config.audio.channels,
            sample_rate: state.mixer_config.audio.sample_rate,
            sample_format: state.mixer_config.audio.sample_format
          },
          synchronize_buffers?: true
        }

        [
          child(:audio_mixer, audio_mixer)
          |> child(:aac_encoder, Membrane.AAC.FDK.Encoder)
          |> child(:aac_parser, %Membrane.AAC.Parser{out_encapsulation: :none})
          |> via_in(Pad.ref(:input, :audio),
            options: [
              encoding: :AAC,
              segment_duration: state.segment_duration,
              partial_segment_duration: state.partial_segment_duration
            ]
          )
          |> get_child(:hls_sink_bin)
        ]
      end
    else
      defp generate_audio_mixer(_state, _ctx) do
        raise """
        Cannot find some of the modules required to use the audio mixer.
        Ensure that the following dependencies are added to the deps.
        #{merge_strings(@audio_mixer_deps)}
        """
      end
    end

    defp get_track_offset(%{stream_beginning: nil} = state),
      do: {0, Map.put(state, :stream_beginning, System.monotonic_time())}

    defp get_track_offset(state), do: {System.monotonic_time() - state.stream_beginning, state}

    defp hls_sink_bin_exists?(track, ctx, %{mixer_config: nil}),
      do: Map.has_key?(ctx.children, {:hls_sink_bin, track.stream_id})

    defp hls_sink_bin_exists?(_track, ctx, _state), do: Map.has_key?(ctx.children, :hls_sink_bin)

    defp get_depayloader(track) do
      track
      |> Track.get_depayloader()
      |> tap(&unless &1, do: raise("Couldn't find depayloader for track #{inspect(track)}"))
    end

    defp get_hls_stream_directory(%{mixer_config: nil} = state, track),
      do: Path.join(state.output_directory, track.stream_id)

    defp get_hls_stream_directory(state, _track), do: state.output_directory

    defp compositor_update_layout(_action, %{type: :audio}, _stream_format, state),
      do: {[], state}

    defp compositor_update_layout(_action, _track, _stream_format, %{mixer_config: nil} = state),
      do: {[], state}

    defp compositor_update_layout(
           action,
           track,
           stream_format,
           %{video_layout_state: video_layout_state} = state
         ) do
      {updated_layout, video_layout_state} =
        case action do
          :add ->
            state.mixer_config.video.layout_module.track_added(
              video_layout_state,
              track,
              stream_format
            )

          :update ->
            state.mixer_config.video.layout_module.track_updated(
              video_layout_state,
              track,
              stream_format
            )

          :remove ->
            state.mixer_config.video.layout_module.track_removed(video_layout_state, track)
        end

      {layouts, transformations} =
        updated_layout
        |> Enum.map(fn {track_id, layout, transformations} ->
          {{Pad.ref(:input, track_id), layout}, {Pad.ref(:input, track_id), transformations}}
        end)
        |> Enum.unzip()

      update_layout_actions = [
        notify_child: {:compositor, {:update_transformations, transformations}},
        notify_child: {:compositor, {:update_placement, layouts}}
      ]

      {update_layout_actions, %{state | video_layout_state: video_layout_state}}
    end

    unless Enum.all?(@compositor_deps ++ @audio_mixer_deps, &Code.ensure_loaded?/1),
      do: defp(merge_strings(strings), do: Enum.join(strings, ", "))
  end
end
