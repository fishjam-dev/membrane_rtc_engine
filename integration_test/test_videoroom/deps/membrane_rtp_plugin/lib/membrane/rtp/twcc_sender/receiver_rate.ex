defmodule Membrane.RTP.TWCCSender.ReceiverRate do
  @moduledoc false
  # module responsible for calculating bitrate
  # received by a receiver in last `window` time
  # (referred to as R_hat in the GCC draft, sec. 5.5)

  alias Membrane.Time

  @type t() :: %__MODULE__{
          # estimated bitrate in bps
          value: float() | nil,
          # time window for measuring the received bitrate, between [0.5, 1]s (reffered to as "T" in the draft)
          window: Time.t(),
          # accumulator for packets and their timestamps that have been received in last `window` time
          packets_received: Qex.t({Time.t(), pos_integer()})
        }

  @enforce_keys [:window]
  defstruct @enforce_keys ++ [:value, packets_received: Qex.new()]

  @spec new(Time.t()) :: t()
  def new(window), do: %__MODULE__{window: window}

  @spec update(t(), Time.t(), [Time.t() | :not_received], [pos_integer()]) :: t()
  def update(%__MODULE__{value: nil} = rr, reference_time, receive_deltas, packet_sizes) do
    packets_received = resolve_receive_deltas(receive_deltas, reference_time, packet_sizes)

    packets_received = Qex.join(rr.packets_received, packets_received)

    {first_packet_timestamp, _first_packet_size} = Qex.first!(packets_received)
    {last_packet_timestamp, _last_packet_size} = Qex.last!(packets_received)

    if last_packet_timestamp - first_packet_timestamp >= rr.window do
      do_update(rr, packets_received)
    else
      %__MODULE__{rr | packets_received: packets_received}
    end
  end

  def update(%__MODULE__{} = rr, reference_time, receive_deltas, packet_sizes) do
    packets_received = resolve_receive_deltas(receive_deltas, reference_time, packet_sizes)
    do_update(rr, packets_received)
  end

  defp do_update(rr, packets_received) do
    {last_packet_timestamp, _last_packet_size} = Qex.last!(packets_received)
    threshold = last_packet_timestamp - rr.window

    packets_received =
      rr.packets_received
      |> Qex.join(packets_received)
      |> Enum.drop_while(fn {timestamp, _size} -> timestamp < threshold end)
      |> Qex.new()

    received_sizes_sum =
      Enum.reduce(packets_received, 0, fn {_timestamp, size}, acc -> acc + size end)

    value = 1 / (Time.as_milliseconds(rr.window) / 1000) * received_sizes_sum

    %__MODULE__{rr | value: value, packets_received: packets_received}
  end

  defp resolve_receive_deltas(receive_deltas, reference_time, packet_sizes) do
    receive_deltas
    |> Enum.zip(packet_sizes)
    |> Enum.reject(fn {delta, _size} -> delta == :not_received end)
    |> Enum.map_reduce(reference_time, fn {recv_delta, size}, prev_timestamp ->
      receive_timestamp = prev_timestamp + recv_delta
      {{receive_timestamp, size}, receive_timestamp}
    end)
    # take the packets_received
    |> elem(0)
    |> Qex.new()
  end
end
