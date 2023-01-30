defmodule Membrane.RTC.Engine.FilterTee do
  @moduledoc false

  # Element for forwarding buffers to multiple output pads. If no output
  # pad is linked, buffers are dropped.

  # It has got built-in mechanism for limiting forwarding video buffers.
  # It reads from ETS table on which pads it should forward buffers.

  # Counter is used for passing a single packets once in a while.
  # It is necessary for SRTP as they can update their ROCs
  # based on sequence numbers and when we drop to many packets we may roll it over.

  use Membrane.Filter

  def_options track_id: [
                spec: String.t(),
                description: "Id of track for which tee was created"
              ],
              ets_name: [
                spec: String.t(),
                description:
                  "Name of ETS table from which Tee will read to which pads it should send buffers",
                default: "table"
              ],
              type: [
                spec: :audio | :video,
                description: "Type of track which buffers tee is forwarding"
              ],
              codec: [
                type: :atom,
                spec: [:H264 | :VP8 | :OPUS],
                description: "Codec of track #{inspect(__MODULE__)} will forward."
              ]

  def_input_pad :input,
    availability: :always,
    mode: :pull,
    demand_mode: :auto,
    accepted_format: _any

  def_output_pad :output,
    availability: :on_request,
    mode: :push,
    accepted_format: _any

  @impl true
  def handle_init(_ctx, opts) do
    state = %{
      ets_name: :"#{opts.ets_name}",
      track_id: opts.track_id,
      counter: 0,
      type: opts.type,
      forward_to: MapSet.new(),
      codec: opts.codec
    }

    {[], state}
  end

  @impl true
  def handle_pad_added(Pad.ref(:output, _ref) = pad, ctx, state) do
    case ctx.pads.input.stream_format do
      nil -> {[], state}
      format -> {[stream_format: {pad, format}], state}
    end
  end

  @impl true
  def handle_process(:input, %Membrane.Buffer{} = buffer, _ctx, %{type: :audio} = state) do
    {[forward: buffer], state}
  end

  @impl true
  def handle_process(
        :input,
        %Membrane.Buffer{} = buffer,
        _ctx,
        %{type: :video, counter: 1000} = state
      ) do
    {[forward: buffer], %{state | counter: 0}}
  end

  @impl true
  def handle_process(:input, %Membrane.Buffer{} = buffer, ctx, %{type: :video} = state) do
    pads =
      ctx.pads
      |> Map.keys()
      |> Enum.filter(fn
        {Membrane.Pad, :output, {:endpoint, _endpoint_id} = endpoint_name} ->
          MapSet.member?(state.forward_to, endpoint_name)

        {Membrane.Pad, :output, _ref} ->
          true

        _other ->
          false
      end)

    actions = Enum.map(pads, &{:buffer, {&1, buffer}})
    {actions, %{state | counter: state.counter + 1}}
  end

  @impl true
  def handle_parent_notification(:track_priorities_updated, _ctx, state) do
    forward_to =
      case :ets.lookup(state.ets_name, state.track_id) do
        [{_track_id, endpoint_names} | _] ->
          MapSet.new(endpoint_names)

        [] ->
          MapSet.new()
      end

    new_forwards = MapSet.difference(forward_to, state.forward_to)

    actions =
      if MapSet.size(new_forwards) != 0,
        do: [event: {:input, %Membrane.KeyframeRequestEvent{}}],
        else: []

    {actions, %{state | forward_to: forward_to}}
  end
end
