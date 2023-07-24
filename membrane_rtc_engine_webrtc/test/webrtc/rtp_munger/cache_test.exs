defmodule Membrane.RTC.Engine.Endpoint.WebRTC.RTPMunger.CacheTest do
  use ExUnit.Case, async: true

  alias Membrane.RTC.Engine.Endpoint.WebRTC.RTPMunger.Cache

  @max_seq_num 2 ** 16
  @history_size 64

  describe "RTPMunger.Cache" do
    test "can retrieve stored information only once" do
      entries =
        1..100
        |> Enum.map(&{&1, &1 + 200})

      cache =
        entries
        |> Qex.new()
        |> then(&%Cache{cache: &1})

      Enum.each(entries, fn {a, b} ->
        assert {:ok, ^b, cache} = Cache.get_and_remove(cache, a)
        assert {:error, :not_found} == Cache.get_and_remove(cache, a)
      end)

      assert {:error, :not_found} == Cache.get_and_remove(cache, 123)
    end

    test "adds entries after rollover if they aren't too old" do
      cache =
        Cache.new()
        |> Cache.push(@max_seq_num - 2, 0)
        |> Cache.push(1, 1)

      assert Enum.to_list(cache.cache) == [{@max_seq_num - 2, 0}, {1, 1}]
    end

    test "removes entries after rollover if they are too old" do
      too_old_seq_num = @max_seq_num - @history_size - 200

      cache =
        Cache.new()
        |> Cache.push(too_old_seq_num - 1, too_old_seq_num)
        |> Cache.push(1, 2)

      assert {:error, :not_found} = Cache.get_and_remove(cache, too_old_seq_num)
    end
  end
end
