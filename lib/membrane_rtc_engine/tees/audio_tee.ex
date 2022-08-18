defmodule Membrane.RTC.Engine.AudioTee do
  @moduledoc false
  use Membrane.Filter

  require Membrane.Logger

  alias Membrane.RTC.Engine.Event.{
    RequestTrackVariant,
    TrackVariantPaused,
    TrackVariantResumed,
    TrackVariantSwitched
  }

  alias Membrane.RTC.Engine.Exception.{RequestTrackVariantError, TrackVariantStateError}

  @supported_codecs [:OPUS]

  def_options track: [
                type: :struct,
                spec: Membrane.RTC.Engine.Track.t(),
                description: "Track this tee is going to forward to other endpoints"
              ]

  def_input_pad :input,
    availability: :on_request,
    mode: :pull,
    demand_mode: :auto,
    caps: Membrane.RTP

  def_output_pad :output,
    availability: :on_request,
    mode: :push,
    caps: Membrane.RTP

  @impl true
  def handle_init(opts) do
    if opts.track.encoding not in @supported_codecs do
      raise("""
      #{inspect(__MODULE__)} does not support codec #{inspect(opts.track.encoding)}.
      Supported codecs: #{inspect(@supported_codecs)}
      """)
    end

    {:ok,
     %{
       track: opts.track,
       routes: MapSet.new(),
       track_active: false
     }}
  end

  @impl true
  def handle_pad_added(Pad.ref(:input, {_track_id, :high}), _ctx, state) do
    {:ok, state}
  end

  @impl true
  def handle_pad_added(
        Pad.ref(:output, {:endpoint, _endpoint_id}) = pad,
        %{playback_state: playback_state},
        state
      ) do
    actions =
      cond do
        playback_state == :playing and state.track_active == true ->
          [
            caps: {pad, %Membrane.RTP{}},
            event: {pad, %TrackVariantResumed{variant: List.first(state.track.variants)}}
          ]

        playback_state == :playing ->
          [caps: {pad, %Membrane.RTP{}}]

        true ->
          []
      end

    {{:ok, actions}, state}
  end

  @impl true
  def handle_pad_added(pad, _ctx, _state) do
    raise("Pad #{inspect(pad)} not allowed for #{inspect(__MODULE__)}")
  end

  @impl true
  def handle_pad_removed(Pad.ref(:output, {:endpoint, _endpoint_id}) = pad, _ctx, state) do
    state = update_in(state, [:routes], &MapSet.delete(&1, pad))
    {:ok, state}
  end

  @impl true
  def handle_pad_removed(Pad.ref(:input, {_track_id, _rid}), _ctx, state) do
    {:ok, state}
  end

  @impl true
  def handle_prepared_to_playing(ctx, state) do
    actions =
      ctx.pads
      |> Enum.filter(fn {_pad, %{name: name}} -> name == :output end)
      |> Enum.flat_map(fn {pad, _pad_data} ->
        [caps: {pad, %Membrane.RTP{}}]
      end)

    {{:ok, actions}, state}
  end

  @impl true
  def handle_event(Pad.ref(:input, _id), %TrackVariantPaused{} = event, _ctx, state) do
    state = %{state | track_active: false}
    {{:ok, forward: event}, state}
  end

  @impl true
  def handle_event(Pad.ref(:input, _id), %TrackVariantResumed{} = event, _ctx, state) do
    state = %{state | track_active: true}
    {{:ok, forward: event}, state}
  end

  @impl true
  def handle_event(
        Pad.ref(:output, {:endpoint, endpoint_id}) = output_pad,
        %RequestTrackVariant{variant: requested_variant},
        _ctx,
        state
      ) do
    cond do
      requested_variant not in state.track.variants ->
        raise RequestTrackVariantError,
          requester: endpoint_id,
          requested_variant: requested_variant,
          track: state.track

      state.track_active == false ->
        Membrane.Logger.debug("""
        Endpoint #{endpoint_id} requested track variant: #{requested_variant} but it is inactive. \
        Ignoring.\
        """)

        {:ok, state}

      true ->
        Membrane.Logger.info(
          "Endpoint #{endpoint_id} requested track variant #{requested_variant}."
        )

        state = update_in(state, [:routes], &MapSet.put(&1, output_pad))
        actions = [event: {output_pad, %TrackVariantSwitched{new_variant: requested_variant}}]
        {{:ok, actions}, state}
    end
  end

  @impl true
  def handle_event(pad, event, ctx, state) do
    super(pad, event, ctx, state)
  end

  @impl true
  def handle_process(Pad.ref(:input, {_track_id, variant}), buffer, _ctx, state) do
    unless state.track_active do
      raise TrackVariantStateError, track: state.track, variant: variant
    end

    actions =
      Enum.flat_map(state.routes, fn output_pad ->
        [buffer: {output_pad, buffer}]
      end)

    {{:ok, actions}, state}
  end

  @impl true
  def handle_end_of_stream(_pad, _ctx, state) do
    {{:ok, forward: :end_of_stream}, state}
  end
end
