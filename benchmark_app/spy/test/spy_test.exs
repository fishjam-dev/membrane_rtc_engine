defmodule SpyTest do
  use ExUnit.Case
  doctest Spy

  test "greets the world" do
    assert Spy.hello() == :world
  end
end
