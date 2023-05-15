defmodule Membrane.RTC.Engine.Endpoint.Compositor do
  @moduledoc false

  use Membrane.Bin

  alias Membrane.RTC.Engine.Endpoint.HLS.{
    AudioMixerConfig,
    CompositorConfig
  }

  alias Membrane.RTC.Engine.Endpoint.WebRTC.{TrackReceiver, TrackSender}
  alias Membrane.RTC.Engine.Track
  alias Membrane.VideoCompositor.RustStructs.BaseVideoPlacement

  @track_children [
    :depayloader,
    :opus_decoder,
    :aac_encoder,
    :aac_parser,
    :video_parser,
    :video_decoder,
    :stream_format_updater
  ]

  @common_children [
    :fake_source,
    :video_realtimer,
    :silence_generator,
    :audio_realtimer,
    :audio_mixer,
    :aac_encoder,
    :aac_parser,
    :compositor,
    :encoder,
    :video_parser_out
  ]

  def_input_pad :input,
    demand_unit: :buffers,
    accepted_format: _any,
    availability: :on_request

  def_options rtc_engine: [
                spec: pid(),
                description: "Pid of parent Engine"
              ],
              synchronize_tracks?: [
                spec: boolean(),
                default: true,
                description: """
                Set to false if source is different than webrtc.
                If set to true HLS Endpoint will calculate track offset based on `handle_pad_added` call.
                """
              ],
              audio_config: [
                spec: AudioMixerConfig.t() | nil,
                default: nil,
                description: """
                Audio mixer configuration. If you don't want to mix audio pass nil.
                """
              ],
              video_config: [
                spec: CompositorConfig.t() | nil,
                default: nil,
                description: """
                Video compositor configuration. If you don't want to compose video pass nil.
                """
              ],
              persist?: [
                spec: boolean(),
                default: false
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
        # refactor this, it should be handled inside compositor_update_layout() or video_layout
        video_layout_tracks_added: %{}
      })

    video_layout_state =
      if is_nil(state.video_config),
        do: nil,
        else: state.video_config.layout_module.init(state.video_config.stream_format)

    {[], %{state | video_layout_state: video_layout_state}}
  end

  @impl true
  def handle_playing(context, state) do
    spec =
      generate_audio_mixer(state, context) ++
        generate_compositor(state, context)

    {[spec: spec], state}
  end

  @impl true
  def handle_pad_added(Pad.ref(:input, track_id) = pad, ctx, state) do
    {offset, state} = get_track_offset(state)

    track = Map.get(state.tracks, track_id)
    track_spec = get_track_spec(offset, bin_input(pad), track, state, ctx)

    {[spec: spec], state}
  end

  @impl true
  def handle_pad_removed(Pad.ref(:input, track_id), context, state) do
    {removed_track, state} = pop_in(state, [:tracks, track_id])

    track_children =
      @track_children
      |> Enum.map(&{&1, track_id})
      |> Enum.filter(&Map.has_key?(ctx.children, &1))

    children_to_remove =
      if state.tracks == %{}, do: track_children, else: track_children ++ @common_children

    {update_layout_action, state} = compositor_update_layout(:remove, removed_track, state)

    state = %{
      state
      | video_layout_tracks_added: Map.delete(state.video_layout_tracks_added, track_id)
    }

    {[remove_chlid: children_to_remove] ++ update_layout_action, state}
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

    {update_layout_action, state} = compositor_update_layout(action, track, state, stream_format)

    result_actions = update_layout_action ++ [notify_child: {child, :layout_updated}]
    state = put_in(state, [:video_layout_tracks_added, track_id], true)

    {result_actions, state}
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
  def handle_parent_notification({:track_metadata_updated, track}, _ctx, state) do
    compositor_update_layout(:update, track, state)
    state = put_in(state, [:tracks, track.id], track)

    {[], state}
  end

  @impl true
  def handle_parent_notification(msg, _ctx, state) do
    Membrane.Logger.warn("Unexpected message: #{inspect(msg)}. Ignoring.")
    {[], state}
  end

  defp generate_compositor(%{video_config: nil}, _ctx), do: []

  defp generate_compositor(%{video_config: video_config}, _ctx) do
    {frames_per_second, 1} = video_config.stream_format.framerate

    compositor = %Membrane.VideoCompositor{
      stream_format: video_config.stream_format
    }

    encoder = %Membrane.H264.FFmpeg.Encoder{
      profile: :baseline,
      tune: :zerolatency,
      gop_size: frames_per_second * video_config.keyframe_frequency
    }

    # TODO change to new parser once it supports Membrane.H264 stream format on input pad
    video_parser_out = %Membrane.H264.FFmpeg.Parser{
      alignment: :au,
      attach_nalus?: true
    }

    [
      child(:compositor, compositor)
      |> child(:encoder, encoder)
      |> child(:video_parser_out, video_parser_out)
      |> get_child(:video_track_sender)
    ]
  end

  defp generate_audio_mixer(%{audio_config: nil}, _ctx), do: []

  defp generate_audio_mixer(%{audio_config: audio_config}, _ctx) do
    audio_mixer = %Membrane.AudioMixer{
      stream_format: audio_config.stream_format,
      synchronize_buffers?: true
    }

    [
      child(:audio_mixer, audio_mixer)
      |> child(:aac_encoder, Membrane.AAC.FDK.Encoder)
      |> child(:aac_parser, %Membrane.AAC.Parser{out_encapsulation: :none})
      |> get_child(:audio_track_sender)
    ]
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

  defp attach_track_spec(offset, %{type: :audio} = track, _state),
    do: [
      get_child({:depayloader, track.id})
      |> child({:opus_decoder, track.id}, Membrane.Opus.Decoder)
      |> child({:audio_filler, track.id}, Membrane.AudioFiller)
      |> via_in(Pad.ref(:input, {:extra, track.id}), options: [offset: offset])
      |> get_child(:audio_mixer)
    ]

  defp attach_track_spec(offset, %{type: :video} = track, _state),
    do: [
      get_child({:depayloader, track.id})
      # TODO change to new parser once it supports Membrane.H264 stream format on input pad
      |> child({:video_parser, track.id}, %Membrane.H264.FFmpeg.Parser{
        attach_nalus?: true,
        alignment: :au
      })
      |> child({:stream_format_updater, track.id}, StreamFormatUpdater)
      |> child({:video_decoder, track.id}, Membrane.H264.FFmpeg.Decoder)
      |> via_in(Pad.ref(:input, track.id),
        # should be behind black frames from blank generator
        options: [initial_placement: @initial_placement, timestamp_offset: offset]
      )
      |> get_child(:compositor)
    ]

  defp get_depayloading_track_spec(link_builder, track),
    do: [
      link_builder
      |> child({:track_receiver, track.id}, %TrackReceiver{
        track: track,
        initial_target_variant: :high
      })
      |> child({:depayloader, track.id}, get_depayloader(track))
    ]

  defp generate_silence_spec(%{audio_config: nil}, ctx), do: []

  defp generate_silence_spec(%{audio_config: audio_config}, _ctx) do
    silence_generator =
      if is_nil(audio_config.background),
        do: get_silence_generator(audio_config.stream_format),
        else: audio_config.background

    [
      child(:silence_generator, silence_generator)
      |> child(:audio_realtimer, Membrane.Realtimer)
      |> get_child(:audio_mixer)
    ]
  end

  defp get_silence_generator(stream_format),
    do: %Membrane.SilenceGenerator{
      stream_format: stream_format,
      duration: :infinity,
      # Value 960 is consistent with what we get from browser also with addition of default stream_format
      # it generates total values so we don't lose data becouse of rounding error
      frames_per_buffer: 960
    }

  defp generate_blank_spec(%{video_config: nil}, ctx), do: []

  defp generate_blank_spec(%{video_config: video_config}, _ctx) do
    background_generator =
      if is_nil(video_config.background),
        do: get_blank_generator(video_config),
        else: video_config.background

    [
      child(:fake_source, background_generator)
      |> child(:video_realtimer, Membrane.Realtimer)
      |> via_in(:input, options: [initial_placement: @initial_placement])
      |> get_child(:compositor)
    ]
  end

  defp get_blank_generator(%{stream_format: stream_format}),
    do: %Membrane.BlankVideoGenerator{
      stream_format: stream_format,
      duration: :infinity
    }

  defp attach_track_spec(offset, %{type: :audio} = track, state),
    do: attach_audio_track_spec(offset, track, state)

  defp attach_track_spec(offset, %{type: :video} = track, state),
    do: attach_video_track_spec(offset, track, state)

  defp compositor_update_layout(_action, _track, _state, _stream_format \\ nil)

  defp compositor_update_layout(_action, %{type: :audio}, state, _stream_format),
    do: {[], state}

  defp compositor_update_layout(_action, _track, %{video_config: nil} = state, _stream_format),
    do: {[], state}

  defp compositor_update_layout(
         action,
         track,
         %{video_config: video_config, video_layout_state: video_layout_state} = state,
         stream_format
       ) do
    {updated_layout, video_layout_state} =
      case action do
        :add ->
          video_config.layout_module.track_added(
            track,
            stream_format,
            video_layout_state
          )

        :update ->
          video_config.layout_module.track_updated(
            track,
            stream_format,
            video_layout_state
          )

        :remove ->
          video_config.layout_module.track_removed(track, video_layout_state)
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
end
