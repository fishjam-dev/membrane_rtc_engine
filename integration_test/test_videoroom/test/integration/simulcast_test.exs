defmodule TestVideoroom.Integration.SimulcastTest do
  use TestVideoroomWeb.ConnCase, async: false

  # in miliseconds
  @peer_delay 500
  # in miliseconds
  @peer_duration 60_000
  @room_url "http://localhost:4001"

  # in miliseconds
  @join_interval 20_000

  @start_with_simulcast "start-simulcast"
  @change_own_low "simulcast-own-low"
  @change_own_medium "simulcast-own-medium"
  @change_own_high "simulcast-own-high"
  @set_peer_encoding_low "simulcast-other-low"
  @set_peer_encoding_medium "simulcast-other-medium"
  @set_peer_encoding_high "simulcast-other-high"
  @simulcast_stats "simulcast-stats"
  @browser_options %{count: 1, delay: @peer_delay, headless: true}

  @moduletag timeout: 180_000
  test "User disable and then enable medium encoding" do
    browsers_number = 2

    pid = self()

    receiver = Process.spawn(fn -> receive_stats(browsers_number, pid) end, [:link])

    mustang_options = %{
      target_url: @room_url,
      linger: @peer_duration,
      join_interval: @join_interval,
      start_button: @start_with_simulcast,
      receiver: receiver,
      buttons: [],
      id: -1
    }

    timeouts = [15_000, 15_000]

    buttons1 = [@change_own_medium, @change_own_medium] |> Enum.zip(timeouts)

    buttons2 = [@simulcast_stats, @simulcast_stats] |> Enum.zip(timeouts)

    stages = [:disable, :enable]

    buttons_with_id =
      [buttons1, buttons2]
      |> Enum.map(&Enum.zip(&1, stages))
      |> Enum.with_index()
      |> Map.new(fn {button, browser_id} -> {browser_id, button} end)

    for {browser_id, buttons} <- buttons_with_id, into: [] do
      specific_mustang = %{mustang_options | id: browser_id, buttons: buttons}

      Process.sleep(2_000)

      Task.async(fn ->
        Stampede.start({SimulcastMustang, specific_mustang}, @browser_options)
      end)
    end
    |> Task.await_many(:infinity)

    receive do
      acc ->
        Enum.all?(acc, fn {stage, browsers} ->
          case stage do
            :disable ->
              Enum.all?(browsers, fn
                {1, stats} ->
                  assert stats_for_encoding?(stats, "l")

                {_other, _stats} ->
                  true
              end)

            :enable ->
              Enum.all?(browsers, fn
                {1, stats} ->
                  assert stats_for_encoding?(stats, "m")

                {_other, _stats} ->
                  true
              end)
          end
        end)
    end
  end

  @moduletag timeout: 180_000
  test "User change encoding to low and then return to medium" do
    browsers_number = 2

    pid = self()

    receiver = Process.spawn(fn -> receive_stats(browsers_number, pid) end, [:link])

    mustang_options = %{
      target_url: @room_url,
      linger: 40_000,
      join_interval: @join_interval,
      start_button: @start_with_simulcast,
      receiver: receiver,
      buttons: [],
      id: -1
    }

    timeouts = [5_000, 15_000, 5_000, 15_000]

    buttons1 =
      [@set_peer_encoding_low, @simulcast_stats, @set_peer_encoding_medium, @simulcast_stats]
      |> Enum.zip(timeouts)

    buttons2 = []

    stages = [1, :disable, 3, :enable]

    buttons_with_id =
      [buttons1, buttons2]
      |> Enum.map(&Enum.zip(&1, stages))
      |> Enum.with_index()
      |> Map.new(fn {button, browser_id} -> {browser_id, button} end)

    for {browser_id, buttons} <- buttons_with_id, into: [] do
      specific_mustang = %{mustang_options | id: browser_id, buttons: buttons}

      Process.sleep(2_000)

      Task.async(fn ->
        Stampede.start({SimulcastMustang, specific_mustang}, @browser_options)
      end)
    end
    |> Task.await_many(:infinity)

    receive do
      acc ->
        Enum.all?(acc, fn {stage, browsers} ->
          case stage do
            :disable ->
              Enum.all?(browsers, fn
                {0, stats} ->
                  assert stats_for_encoding?(stats, "l")

                {_other, _stats} ->
                  true
              end)

            :enable ->
              Enum.all?(browsers, fn
                {0, stats} ->
                  assert stats_for_encoding?(stats, "m")

                {_other, _stats} ->
                  true
              end)
          end
        end)
    end
  end

  @moduletag timeout: 180_000
  test "User gradually disable all encodings and then gradually enable encodings" do
    browsers_number = 2

    pid = self()

    receiver = Process.spawn(fn -> receive_stats(browsers_number, pid) end, [:link])

    mustang_options = %{
      target_url: @room_url,
      linger: @peer_duration,
      join_interval: @join_interval,
      start_button: @start_with_simulcast,
      receiver: receiver,
      buttons: [],
      id: -1
    }

    timeouts = [15_000, 15_000, 15_000, 15_000]

    buttons1 =
      [@change_own_medium, @change_own_low, @change_own_low, @change_own_medium]
      |> Enum.zip(timeouts)

    buttons2 =
      [@simulcast_stats, @simulcast_stats, @simulcast_stats, @simulcast_stats]
      |> Enum.zip(timeouts)

    stages = [:disable_medium, :disable_low, :enable_low, :enable_medium]

    buttons_with_id =
      [buttons1, buttons2]
      |> Enum.map(&Enum.zip(&1, stages))
      |> Enum.with_index()
      |> Map.new(fn {button, browser_id} -> {browser_id, button} end)

    for {browser_id, buttons} <- buttons_with_id, into: [] do
      specific_mustang = %{mustang_options | id: browser_id, buttons: buttons}

      Process.sleep(1_500)

      Task.async(fn ->
        Stampede.start({SimulcastMustang, specific_mustang}, @browser_options)
      end)
    end
    |> Task.await_many(:infinity)

    receive do
      acc ->
        Enum.all?(acc, fn {stage, browsers} ->
          case stage do
            :disable_medium ->
              Enum.all?(browsers, fn
                {1, stats} ->
                  assert stats_for_encoding?(stats, "l")

                {_other, _stats} ->
                  true
              end)

            :disable_low ->
              Enum.all?(browsers, fn
                {1, stats} ->
                  assert stats["framesPerSecond"] == 0

                {_other, _stats} ->
                  true
              end)

            :enable_low ->
              Enum.all?(browsers, fn
                {1, stats} ->
                  assert stats_for_encoding?(stats, "l")

                {_other, _stats} ->
                  true
              end)

            :enable_medium ->
              Enum.all?(browsers, fn
                {1, stats} ->
                  assert stats_for_encoding?(stats, "m")

                {_other, _stats} ->
                  true
              end)
          end
        end)
    end
  end

  defp stats_for_encoding?(stats, "l") do
    stats["height"] == 180 and stats["width"] == 320 and
      stats["callbackEncoding"] == "l"
  end

  defp stats_for_encoding?(stats, "m") do
    stats["height"] == 360 and stats["width"] == 640 and
      stats["callbackEncoding"] == "m"
  end

  defp stats_for_encoding?(stats, "h") do
    stats["height"] == 720 and stats["width"] == 1280 and
      stats["callbackEncoding"] == "h"
  end

  defp receive_stats(mustangs_number, pid, acc \\ %{}) do
    if mustangs_number > 0 do
      receive do
        {_browser_id, :end} ->
          receive_stats(mustangs_number - 1, pid, acc)

        {browser_id, stage, data} ->
          acc
          |> then(fn acc ->
            default_map = %{browser_id => data}
            Map.update(acc, stage, default_map, &Map.put(&1, browser_id, data))
          end)
          |> then(&receive_stats(mustangs_number, pid, &1))
      end
    else
      send(pid, acc)
    end
  end
end
