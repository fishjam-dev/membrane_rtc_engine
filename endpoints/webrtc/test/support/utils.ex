defmodule Membrane.RTC.Engine.Support.Utils do
  @moduledoc false

  alias Membrane.Buffer

  @spec generate_random_payload(non_neg_integer) :: binary()
  def generate_random_payload(size) when size >= 0 do
    for _i <- 1..size, into: <<>>, do: <<Enum.random(0..255)>>
  end

  @spec generator(any(), non_neg_integer()) :: {[Membrane.Element.Action.t()], any()}
  def generator(state, buffers_cnt) do
    payload = <<1, 2, 3, 4, 5>>
    metadata = %{is_keyframe: false}

    buffers =
      for _i <- 1..buffers_cnt do
        %Buffer{payload: payload, metadata: metadata}
      end

    {[buffer: {:output, buffers}], state}
  end
end
