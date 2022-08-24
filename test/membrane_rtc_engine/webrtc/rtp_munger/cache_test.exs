defmodule Membrane.RTC.Engine.Endpoint.WebRTC.RTPMunger.CacheTest do
  use ExUnit.Case, async: true

  alias Membrane.RTC.Engine.Endpoint.WebRTC.RTPMunger.Cache

  @max_seq_num 2 ** 16
  @history_size div(@max_seq_num, 2)

  describe "Membrane.RTC.Engine.Endpoint.WebRTC.RTPMunger.Cache" do
    test "can retrieve stored information" do
      entries =
        1..100
        |> Enum.map(&{&1, &1 + 200})

      cache =
        entries
        |> Qex.new()
        |> then(&%Cache{cache: &1})

      Enum.each(entries, fn {a, b} ->
        assert Cache.get(cache, a) == {:ok, b}
      end)

      assert {:error, :not_found} == Cache.get(cache, 123)
    end

    test "can store information if they aren't too old" do
      cache =
        Cache.new()
        |> Cache.push(@max_seq_num - 2, 0)
        |> Cache.push(1, 1)

      assert Enum.to_list(cache.cache) == [{@max_seq_num - 2, 0}, {1, 1}]
    end

    test "correctly removes too old entries" do
      too_old_seq_num = @max_seq_num - @history_size - 200

      cache =
        Cache.new()
        |> Cache.push(too_old_seq_num, 0)
        |> Cache.push(1, 20)

      assert {:error, :not_found} = Cache.get(cache, too_old_seq_num)
    end
  end
end
