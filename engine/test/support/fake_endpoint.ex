defmodule Membrane.RTC.Engine.Support.SinkEndpoint do
  @moduledoc false

  # Endpoint that subscribes on all published tracks and drops them.

  use Membrane.Bin

  require Membrane.Logger

  alias Membrane.Fake.Sink
  alias Membrane.RTC.Engine

  def_options rtc_engine: [
                spec: pid(),
                description: "Pid of parent Engine"
              ],
              owner: [
                spec: pid(),
                description: "Pid of endpoint owner"
              ]

  @type encoding_t() :: String.t()

  def_input_pad :input,
    accepted_format: _any,
    availability: :on_request

  @impl true
  def handle_init(_ctx, opts) do
    state =
      opts
      |> Map.from_struct()
      |> Map.merge(%{
        subscribing_tracks: %{}
      })

    {[notify_parent: :ready], state}
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
    {[remove_children: {:fake_sink, track_id}], state}
  end

  @impl true
  def handle_parent_notification({:new_tracks, tracks}, ctx, state) do
    {:endpoint, endpoint_id} = ctx.name

    new_subscribing_tracks =
      Map.new(tracks, fn track ->
        ref = Engine.subscribe_async(state.rtc_engine, endpoint_id, track.id)
        {ref, endpoint_id}
      end)

    state = update_in(state.subscribing_tracks, &Map.merge(&1, new_subscribing_tracks))

    {[], state}
  end

  @impl true
  def handle_parent_notification(_msg, _ctx, state) do
    {[], state}
  end

  @impl true
  def handle_info({:subscribe_result, subscribe_ref, result}, _ctx, state) do
    :ok = result
    {endpoint_id, subscribing_tracks} = Map.pop(state.subscribing_tracks, subscribe_ref)

    send(state.owner, endpoint_id)

    {[], %{state | subscribing_tracks: subscribing_tracks}}
  end
end
