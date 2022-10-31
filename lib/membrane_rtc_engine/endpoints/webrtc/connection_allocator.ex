defmodule Membrane.RTC.Engine.Endpoint.WebRTC.ConnectionAllocator do
  @moduledoc """
  Behavior defining a set of functions for managing connection allocations for TrackReceivers.

  It is responsible for allocating connection bandwidth for track receivers and probing the connection.

  # Message protocol

  In addition to implementing the behavior, Each implementation of ConnectionAllocator should be aware of the
  following message protocol.

  `Membrane.RTC.Engine.Endpoint.WebRTC.TrackReceiver` already implements proper handling of this message protocol.

  ## Granting an allocation

  `t:#{inspect(__MODULE__)}.AllocationGrantedNotification.t/0` message is sent to TrackReceiver when ConnectionAllocator
  wants to grant an allocation. TrackReceiver is obligated to comply. This message can also be used to
  force-lower the allocation of a particular TrackReceiver, skipping the negotiation flow.

  ## Negotiating lower allocation
  Whenever it is deemed necessary, ConnectionAllocator can send `t:decrease_allocation_request/0` to
  selected TrackReceiver, to request that it lowers its allocation by unspecified amount.
  Usually, this will mean that TrackReceiver selects a lower variant (if possible).
  In response, TrackReceiver sends `t:decrease_allocation_request_response/0` to ConnectionAllocator.
  If the request was accepted, TrackReceiver is obligated to request lower allocation
  immediately after sending a reply.

  ### Example of decrease allocation request message flow
  ```
  CA - Connection Allocator
  TR - Track Receiver

  CA -> TR: :decrease_your_allocation
  TR -> CA: {:decrease_allocation_request, :accept}
  TR calls `request_allocation/2` with lowered allocation
  CA -> AllocationGrantedNotification
  ```
  """

  alias Membrane.Buffer
  alias Membrane.RTC.Engine.Track

  @typedoc """
  Type describing a message sent by ConnectionAllocator to `Membrane.RTC.Engine.Endpoint.WebRTC.TrackReceiver`
  to request that they lower their allocation.
  """
  @type decrease_allocation_request() :: :decrease_your_allocation

  @typedoc """
  Type describing a message sent by  `Membrane.RTC.Engine.Endpoint.WebRTC.TrackReceiver` to ConnectionAllocator
  in response to `t:decrease_allocation_request/0`, in order to either accept or reject the request.
  """
  @type decrease_allocation_request_response() ::
          {:decrease_allocation_request, :accept | :reject}

  @doc """
  A function that should be used by the WebRTC Endpoint to create an instance of ConnectionAllocator
  """
  @callback start_link() :: GenServer.on_start()

  @doc """
  Function invoked by the TrackReceiver whenever a buffer is sent
  """
  @callback buffer_sent(pid(), Buffer.t()) :: :ok

  @doc """
  Function invoked by the TrackReceiver whenever a padding packet is sent
  """
  @callback probe_sent(pid()) :: :ok

  @doc """
  Function called by the TrackReceiver to register itself in the allocator
  """
  @callback register_track_receiver(pid(), number(), Track.t(), Keyword.t()) :: :ok

  @doc """
  A function called by the endpoint, to update the bandwidth estimation in the allocator
  """
  @callback update_bandwidth_estimation(pid(), number()) :: :ok

  @doc """
  A function used by the VariantSelector to request a different bandwidth allocation.

  In response, the Allocator sends `Membrane.RTC.Engine.Endpoint.WebRTC.ConnectionAllocator.AllocationGrantedNotification`
  to the track receiver that gets new allocation.
  """
  @callback request_allocation(pid(), number()) :: :ok

  @doc """
  A function used by VariantSelector to change it's negotiability status.

  TrackReceiver is considered negotiable if it is both capable of decreasing it's bandwidth usage and, in principal, allowed to do so.
  """
  @callback set_negotiability_status(pid(), boolean()) :: :ok
end
