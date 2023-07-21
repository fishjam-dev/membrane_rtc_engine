defmodule Membrane.RTC.Engine.Support.FakeEndpoint do
  @moduledoc false

  # Endpoint that subscribes on all published tracks and drops them.

  use Membrane.Bin

  require Membrane.Logger

  alias Membrane.Fake.Sink
  alias Membrane.RTC.Engine

  def_options(
    rtc_engine: [
      spec: pid(),
      description: "Pid of parent Engine"
    ],
    owner: [
      spec: pid(),
      description: "Pid of endpoint owner"
    ]
  )

  @type encoding_t() :: String.t()

  def_input_pad(:input,
    demand_unit: :buffers,
    accepted_format: _any,
    availability: :on_request
  )

  @impl true
  def handle_init(_ctx, opts) do
    {[notify_parent: :ready], Map.from_struct(opts)}
  end

  @impl true
  def handle_pad_added(Pad.ref(:input, track_id) = pad, _ctx, state) do
    spec = [
      bin_input(pad)
      |> child({:fake_sink, track_id}, Sink.Buffers)
    ]

    {[spec: spec], state}
  end

  @impl true
  def handle_pad_removed(Pad.ref(:input, track_id), _ctx, state) do
    {[remove_child: {:fake_sink, track_id}], state}
  end

  @impl true
  def handle_parent_notification({:new_tracks, tracks}, ctx, state) do
    {:endpoint, endpoint_id} = ctx.name

    Enum.each(tracks, fn track ->
      :ok = Engine.subscribe(state.rtc_engine, endpoint_id, track.id)
      send(state.owner, endpoint_id)
    end)

    {[], state}
  end

  @impl true
  def handle_parent_notification(_msg, _ctx, state) do
    {[], state}
  end
end
