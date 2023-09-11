defmodule Membrane.RTP.Utils do
  @moduledoc false

  alias Membrane.RTP.Packet

  @spec strip_padding(binary, padding_present? :: boolean) ::
          {:ok, {binary, padding_size :: non_neg_integer()}} | :error
  def strip_padding(binary, padding_present?)
  def strip_padding(binary, false), do: {:ok, {binary, 0}}

  def strip_padding(binary, true) do
    with size when size > 0 <- byte_size(binary),
         padding_size = :binary.last(binary),
         payload_size = byte_size(binary) - padding_size,
         <<stripped_payload::binary-size(payload_size), _::binary-size(padding_size)>> <- binary do
      {:ok, {stripped_payload, padding_size}}
    else
      _error -> :error
    end
  end

  @spec generate_padding(Packet.padding_size()) :: binary()
  def generate_padding(0), do: <<>>

  def generate_padding(padding_size) when padding_size in 1..255 do
    zeros_no = padding_size - 1
    <<0::size(zeros_no)-unit(8), padding_size>>
  end

  @spec from_which_rollover(number() | nil, number(), number()) :: :current | :previous | :next
  def from_which_rollover(previous_value, new_value, rollover_length)

  def from_which_rollover(nil, _new, _rollover_length), do: :current

  def from_which_rollover(previous_value, new_value, rollover_length) do
    # a) current rollover
    distance_if_current = abs(previous_value - new_value)
    # b) new_value is from the previous rollover
    distance_if_previous = abs(previous_value - (new_value - rollover_length))
    # c) new_value is in the next rollover
    distance_if_next = abs(previous_value - (new_value + rollover_length))

    [
      {:current, distance_if_current},
      {:previous, distance_if_previous},
      {:next, distance_if_next}
    ]
    |> Enum.min_by(fn {_atom, distance} -> distance end)
    |> then(fn {result, _value} -> result end)
  end
end
