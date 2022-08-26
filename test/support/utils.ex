defmodule Membrane.RTC.Engine.Support.Utils do
  @moduledoc false

  @spec generate_random_payload(non_neg_integer) :: binary()
  def generate_random_payload(size) when size >= 0 do
    for _i <- 1..size, into: <<>>, do: <<Enum.random(0..255)>>
  end
end
