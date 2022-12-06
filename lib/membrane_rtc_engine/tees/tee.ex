defmodule Membrane.RTC.Engine.Tee do
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

  @supported_codecs [:H264, :VP8, :OPUS]

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
       routes: %{},
       inactive_variants: MapSet.new(opts.track.variants)
     }}
  end

  @impl true
  def handle_pad_added(Pad.ref(:input, {_track_id, _variant}), _ctx, state) do
    {:ok, state}
  end

  @impl true
  def handle_pad_added(
        Pad.ref(:output, {:endpoint, _endpoint_id}) = pad,
        %{playback_state: playback_state},
        state
      ) do
    state =
      put_in(state, [:routes, pad], %{
        target_variant: nil,
        current_variant: nil
      })

    actions =
      if playback_state == :playing do
        actions =
          state
          |> active_variants()
          |> Enum.flat_map(&[event: {pad, %TrackVariantResumed{variant: &1}}])

        [caps: {pad, %Membrane.RTP{}}] ++ actions
      else
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
    {_route, state} = pop_in(state, [:routes, pad])
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
  def handle_event(Pad.ref(:input, id), %TrackVariantPaused{} = event, _ctx, state) do
    {_track_id, variant} = id
    state = Map.update!(state, :inactive_variants, &MapSet.put(&1, variant))

    # reset all target variants set to `variant`
    # when `variant` becomes active again
    # endpoints are expected to request it once again
    state =
      Enum.reduce(state.routes, state, fn
        {output_pad, %{target_variant: ^variant}}, state ->
          put_in(state, [:routes, output_pad, :target_variant], nil)

        {output_pad, %{current_variant: ^variant}}, state ->
          put_in(state, [:routes, output_pad, :current_variant], nil)

        _route, state ->
          state
      end)

    {{:ok, forward: event}, state}
  end

  @impl true
  def handle_event(Pad.ref(:input, id), %TrackVariantResumed{} = event, _ctx, state) do
    {_track_id, variant} = id
    state = Map.update!(state, :inactive_variants, &MapSet.delete(&1, variant))
    {{:ok, forward: event}, state}
  end

  @impl true
  def handle_event(
        Pad.ref(:output, {:endpoint, endpoint_id}) = output_pad,
        %RequestTrackVariant{variant: requested_variant} = event,
        _ctx,
        state
      ) do
    cond do
      requested_variant not in state.track.variants ->
        raise RequestTrackVariantError,
          requester: endpoint_id,
          requested_variant: requested_variant,
          track: state.track

      requested_variant in state.inactive_variants ->
        Membrane.Logger.debug("""
        Endpoint #{endpoint_id} requested track variant: #{requested_variant} but it is inactive. \
        Ignoring.\
        """)

        {:ok, state}

      true ->
        Membrane.Logger.debug("""
        Endpoint #{endpoint_id} requested track variant #{requested_variant}. \
        Requesting keyframe.
        """)

        handle_track_variant_request(output_pad, event, state)
    end
  end

  @impl true
  def handle_event(
        Pad.ref(:output, {:endpoint, endpoint_id}) = pad,
        %Membrane.KeyframeRequestEvent{} = event,
        _ctx,
        state
      ) do
    %{current_variant: current_variant} = state.routes[pad]

    if current_variant do
      Membrane.Logger.warn("""
      Endpoint #{endpoint_id} requested keyframe but we don't send any variant to it. Ignoring.
      """)

      {{:ok, event: {Pad.ref(:input, {state.track.id, current_variant}), event}}, state}
    else
      {:ok, state}
    end
  end

  @impl true
  def handle_event(pad, event, ctx, state) do
    super(pad, event, ctx, state)
  end

  @impl true
  def handle_process(Pad.ref(:input, {_track_id, variant}), buffer, ctx, state) do
    if variant in state.inactive_variants do
      raise TrackVariantStateError, track: state.track, variant: variant
    end

    {actions, state} =
      Enum.flat_map_reduce(state.routes, state, fn route, state ->
        handle_route(buffer, variant, route, ctx, state)
      end)

    {{:ok, actions}, state}
  end

  @impl true
  def handle_end_of_stream(_pad, ctx, state) do
    all_end_of_streams? =
      ctx.pads
      |> Enum.filter(fn {_pad_name, pad_data} ->
        pad_data.direction == :input
      end)
      |> Enum.all?(fn {_pad_name, pad_data} ->
        pad_data.end_of_stream?
      end)

    if all_end_of_streams? do
      {{:ok, forward: :end_of_stream}, state}
    else
      {:ok, state}
    end
  end

  defp handle_track_variant_request(output_pad, event, state) do
    %RequestTrackVariant{variant: requested_variant} = event
    pad = Pad.ref(:input, {state.track.id, requested_variant})
    actions = [event: {pad, %Membrane.KeyframeRequestEvent{}}]
    state = put_in(state, [:routes, output_pad, :target_variant], requested_variant)
    {{:ok, actions}, state}
  end

  defp handle_route(buffer, variant, {output_pad, %{current_variant: variant}}, ctx, state) do
    started? = ctx.pads[output_pad].start_of_stream?

    actions =
      cond do
        started? ->
          [buffer: {output_pad, buffer}]

        buffer.metadata.is_keyframe ->
          event = %TrackVariantSwitched{new_variant: variant}
          [event: {output_pad, event}, buffer: {output_pad, buffer}]

        true ->
          []
      end

    {actions, state}
  end

  defp handle_route(buffer, variant, {output_pad, %{target_variant: variant}}, _ctx, state) do
    if buffer.metadata.is_keyframe do
      state =
        state
        |> put_in([:routes, output_pad, :current_variant], variant)
        |> put_in([:routes, output_pad, :target_variant], nil)

      event = %TrackVariantSwitched{new_variant: variant}
      actions = [event: {output_pad, event}, buffer: {output_pad, buffer}]
      {actions, state}
    else
      {[], state}
    end
  end

  defp handle_route(_buffer, _variant, _route, _ctx, state), do: {[], state}

  defp active_variants(state),
    do:
      state.track.variants
      |> MapSet.new()
      |> MapSet.difference(state.inactive_variants)
end
