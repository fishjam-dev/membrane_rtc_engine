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
  @simulcast_inbound_stats "simulcast-inbound-stats"
  @simulcast_outbound_stats "simulcast-outbound-stats"
  @browser_options %{count: 1, delay: @peer_delay, headless: true}
  @start_buttons [@change_own_low, @change_own_medium, @change_own_high]
  @step_duration 35_000
  @test_duration 300_000

  @moduletag timeout: @test_duration
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
      receiver_button: @simulcast_inbound_stats,
      sender_button: @simulcast_outbound_stats,
      start_buttons: [],
      id: -1
    }

    timeouts = List.duplicate(@step_duration, 2)

    buttons1 =
      [
        @change_own_medium,
        @change_own_medium
      ]
      |> Enum.zip(timeouts)

    buttons2 =
      [
        @simulcast_inbound_stats,
        @simulcast_inbound_stats
      ]
      |> Enum.zip(timeouts)

    stages = [:disable, :enable]

    create_room(mustang_options)

    buttons_with_id =
      [buttons1, buttons2]
      |> Enum.map(&Enum.zip(&1, stages))
      |> Enum.with_index()
      |> Map.new(fn {button, browser_id} -> {browser_id, button} end)

    for {browser_id, buttons} <- buttons_with_id, into: [] do
      start_buttons = if browser_id == 1, do: @start_buttons, else: []

      specific_mustang = %{
        mustang_options
        | id: browser_id,
          buttons: buttons,
          start_buttons: start_buttons
      }

      Task.async(fn ->
        Stampede.start({SimulcastMustang, specific_mustang}, @browser_options)
      end)
    end
    |> Task.await_many(:infinity)

    receive do
      acc ->
        for stage <- stages,
            browsers = Map.get(acc, stage) do
          encoding =
            case stage do
              :disable -> "h"
              :enable -> "m"
            end

          sender = browsers[0]
          receiver = browsers[1]
          stats = Enum.zip(sender, receiver)

          assert(
            Enum.any?(stats, fn {sender, receiver} ->
              stats_for_encoding?(receiver, sender, encoding)
            end),
            "Failed on stage: #{stage} should be encoding: #{encoding},
                receiver stats are: #{inspect(receiver)}
                sender stats are: #{inspect(Enum.map(sender, & &1[encoding]))}
                "
          )
        end
    end
  end

  @moduletag timeout: @test_duration
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
      receiver_button: @simulcast_inbound_stats,
      sender_button: @simulcast_outbound_stats,
      start_buttons: [],
      id: -1
    }

    create_room(mustang_options)

    timeouts = List.duplicate(@step_duration, 2)

    buttons1 =
      [
        @set_peer_encoding_low,
        @set_peer_encoding_medium
      ]
      |> Enum.zip(timeouts)

    buttons2 =
      [
        @simulcast_outbound_stats,
        @simulcast_outbound_stats
      ]
      |> Enum.zip(timeouts)

    stages = [:disable, :enable]

    buttons_with_id =
      [buttons1, buttons2]
      |> Enum.map(&Enum.zip(&1, stages))
      |> Enum.with_index()
      |> Map.new(fn {button, browser_id} -> {browser_id, button} end)

    for {browser_id, buttons} <- buttons_with_id, into: [] do
      start_buttons = if browser_id == 0, do: @start_buttons, else: []

      specific_mustang = %{
        mustang_options
        | id: browser_id,
          buttons: buttons,
          start_buttons: start_buttons
      }

      Task.async(fn ->
        Stampede.start({SimulcastMustang, specific_mustang}, @browser_options)
      end)
    end
    |> Task.await_many(:infinity)

    receive do
      acc ->
        for stage <- stages,
            browsers = Map.get(acc, stage) do
          encoding =
            case stage do
              :disable -> "l"
              :enable -> "m"
            end

          receiver = browsers[0]
          sender = browsers[1]
          stats = Enum.zip(sender, receiver)

          assert(
            Enum.any?(stats, fn {sender, receiver} ->
              stats_for_encoding?(receiver, sender, encoding)
            end),
            "Failed on stage: #{stage} should be encoding: #{encoding},
                receiver stats are: #{inspect(receiver)}
                sender stats are: #{inspect(Enum.map(sender, & &1[encoding]))}
                "
          )
        end
    end
  end

  @moduletag timeout: @test_duration
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
      receiver_button: @simulcast_inbound_stats,
      sender_button: @simulcast_outbound_stats,
      start_buttons: [],
      id: -1
    }

    create_room(mustang_options)

    timeouts = List.duplicate(@step_duration, 6)

    buttons1 =
      [
        @change_own_medium,
        @change_own_high,
        @change_own_low,
        @change_own_low,
        @change_own_high,
        @change_own_medium
      ]
      |> Enum.zip(timeouts)

    buttons2 =
      [
        @simulcast_inbound_stats,
        @simulcast_inbound_stats,
        @simulcast_inbound_stats,
        @simulcast_inbound_stats,
        @simulcast_inbound_stats,
        @simulcast_inbound_stats
      ]
      |> Enum.zip(timeouts)

    stages = [
      :disable_medium,
      :disable_high,
      :disable_low,
      :enable_low,
      :enable_high,
      :enable_medium
    ]

    buttons_with_id =
      [buttons1, buttons2]
      |> Enum.map(&Enum.zip(&1, stages))
      |> Enum.with_index()
      |> Map.new(fn {button, browser_id} -> {browser_id, button} end)

    for {browser_id, buttons} <- buttons_with_id, into: [] do
      start_buttons = if browser_id == 1, do: @start_buttons, else: []

      specific_mustang = %{
        mustang_options
        | id: browser_id,
          buttons: buttons,
          start_buttons: start_buttons
      }

      Task.async(fn ->
        Stampede.start({SimulcastMustang, specific_mustang}, @browser_options)
      end)
    end
    |> Task.await_many(:infinity)

    receive do
      acc ->
        for stage <- stages,
            browsers = Map.get(acc, stage) do
          encoding =
            case stage do
              :disable_medium -> "h"
              :disable_high -> "l"
              :disable_low -> nil
              :enable_low -> "l"
              :enable_high -> "h"
              :enable_medium -> "m"
            end

          sender = browsers[0]
          receiver = browsers[1]

          stats = Enum.zip(sender, receiver)

          case stage do
            :disable_low ->
              assert(
                Enum.any?(receiver, &(&1["framesPerSecond"] == 0)),
                "Failed on stage: #{stage} should be all encodings disabled,
                receiver stats are #{inspect(receiver)}
                sender stats are #{inspect(sender)}
                "
              )

            _other ->
              assert(
                Enum.any?(stats, fn {sender, receiver} ->
                  stats_for_encoding?(receiver, sender, encoding)
                end),
                "Failed on stage: #{stage} should be encoding: #{encoding},
                receiver stats are: #{inspect(receiver)}
                sender stats are: #{inspect(Enum.map(sender, & &1[encoding]))}
                "
              )
          end
        end
    end
  end

  defp create_room(mustang_options) do
    # Creating room earlier to avoid error :already_started
    Task.async(fn ->
      Stampede.start({RoomMustang, mustang_options}, @browser_options)
    end)
    |> Task.await(:infinity)

    Process.sleep(2_000)
  end

  defp stats_for_encoding?(receiver, sender_stats, encoding) do
    sender = sender_stats[encoding]

    correct_dimensions? =
      receiver["height"] == sender["height"] and receiver["width"] == sender["width"]

    (correct_dimensions? or sender["qualityLimitationReason"] != "none") and
      receiver["callbackEncoding"] == encoding
  end

  defp receive_stats(mustangs_number, pid, acc \\ %{}) do
    if mustangs_number > 0 do
      receive do
        {_browser_id, :end} ->
          receive_stats(mustangs_number - 1, pid, acc)

        {browser_id, stage, data} ->
          acc
          |> then(fn acc ->
            default_map = %{browser_id => [data]}

            Map.update(
              acc,
              stage,
              default_map,
              fn stage_map -> Map.update(stage_map, browser_id, [data], &(&1 ++ [data])) end
            )
          end)
          |> then(&receive_stats(mustangs_number, pid, &1))
      end
    else
      send(pid, acc)
    end
  end
end
