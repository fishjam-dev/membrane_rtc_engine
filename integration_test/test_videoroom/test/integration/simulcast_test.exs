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
  @change_own_low "simulcast-local-low-encoding"
  @change_own_medium "simulcast-local-medium-encoding"
  @change_own_high "simulcast-local-high-encoding"
  @set_peer_encoding_low "simulcast-peer-low-encoding"
  @set_peer_encoding_medium "simulcast-peer-medium-encoding"
  @simulcast_inbound_stats "simulcast-inbound-stats"
  @simulcast_outbound_stats "simulcast-outbound-stats"
  @browser_options %{count: 1, delay: @peer_delay, headless: true}
  @step_duration 20_000
  @test_duration 240_000

  @tag timeout: @test_duration
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
      simulcast_inbound_stats_button: @simulcast_inbound_stats,
      simulcast_outbound_stats_button: @simulcast_outbound_stats,
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

    stage_to_expected_encoding = %{disable: "h", enable: "m"}

    create_room(mustang_options)

    buttons_with_id =
      [buttons1, buttons2]
      |> Enum.map(&Enum.zip(&1, stages))
      |> Enum.with_index()
      |> Map.new(fn {button, browser_id} -> {browser_id, button} end)

    for {browser_id, buttons} <- buttons_with_id, into: [] do
      specific_mustang = %{
        mustang_options
        | id: browser_id,
          buttons: buttons
      }

      Task.async(fn ->
        Stampede.start({SimulcastMustang, specific_mustang}, @browser_options)
      end)
    end
    |> Task.await_many(:infinity)

    receive do
      accumulated_stats ->
        for stage <- stages,
            browser_id_to_iterated_stats = Map.get(accumulated_stats, stage) do
          encoding = stage_to_expected_encoding[stage]

          sender_iterated_stats = browser_id_to_iterated_stats[0]
          receiver_iterated_stats = browser_id_to_iterated_stats[1]
          stats = Enum.zip(sender_iterated_stats, receiver_iterated_stats)

          assert(
            Enum.any?(stats, fn {all_encodings_sender_stats, receiver_stats} ->
              stats_for_encoding?(receiver_stats, all_encodings_sender_stats, encoding)
            end),
            "Failed on stage: #{stage} should be encoding: #{encoding},
                receiver stats are: #{inspect(receiver_iterated_stats)}
                sender stats are: #{inspect(Enum.map(sender_iterated_stats, & &1[encoding]))}
                "
          )
        end
    end
  end

  @tag timeout: @test_duration
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
      simulcast_inbound_stats_button: @simulcast_inbound_stats,
      simulcast_outbound_stats_button: @simulcast_outbound_stats,
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

    stage_to_expected_encoding = %{disable: "l", enable: "m"}

    buttons_with_id =
      [buttons1, buttons2]
      |> Enum.map(&Enum.zip(&1, stages))
      |> Enum.with_index()
      |> Map.new(fn {button, browser_id} -> {browser_id, button} end)

    for {browser_id, buttons} <- buttons_with_id, into: [] do
      specific_mustang = %{
        mustang_options
        | id: browser_id,
          buttons: buttons
      }

      Task.async(fn ->
        Stampede.start({SimulcastMustang, specific_mustang}, @browser_options)
      end)
    end
    |> Task.await_many(:infinity)

    receive do
      accumulated_stats ->
        for stage <- stages,
            browser_id_to_iterated_stats = Map.get(accumulated_stats, stage) do
          encoding = stage_to_expected_encoding[stage]

          sender_iterated_stats = browser_id_to_iterated_stats[1]
          receiver_iterated_stats = browser_id_to_iterated_stats[0]
          stats = Enum.zip(sender_iterated_stats, receiver_iterated_stats)

          assert(
            Enum.any?(stats, fn {all_encodings_sender_stats, receiver_stats} ->
              stats_for_encoding?(receiver_stats, all_encodings_sender_stats, encoding)
            end),
            "Failed on stage: #{stage} should be encoding: #{encoding},
                receiver stats are: #{inspect(receiver_iterated_stats)}
                sender stats are: #{inspect(Enum.map(sender_iterated_stats, & &1[encoding]))}
                "
          )
        end
    end
  end

  @tag timeout: @test_duration
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
      simulcast_inbound_stats_button: @simulcast_inbound_stats,
      simulcast_outbound_stats_button: @simulcast_outbound_stats,
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

    stage_to_expected_encoding = %{
      disable_medium: "h",
      disable_high: "l",
      disable_low: nil,
      enable_low: "l",
      enable_high: "h",
      enable_medium: "m"
    }

    buttons_with_id =
      [buttons1, buttons2]
      |> Enum.map(&Enum.zip(&1, stages))
      |> Enum.with_index()
      |> Map.new(fn {button, browser_id} -> {browser_id, button} end)

    for {browser_id, buttons} <- buttons_with_id, into: [] do
      specific_mustang = %{
        mustang_options
        | id: browser_id,
          buttons: buttons
      }

      Task.async(fn ->
        Stampede.start({SimulcastMustang, specific_mustang}, @browser_options)
      end)
    end
    |> Task.await_many(:infinity)

    receive do
      accumulated_stats ->
        for stage <- stages,
            browser_id_to_iterated_stats = Map.get(accumulated_stats, stage) do
          encoding = stage_to_expected_encoding[stage]

          sender_iterated_stats = browser_id_to_iterated_stats[0]
          receiver_iterated_stats = browser_id_to_iterated_stats[1]
          stats = Enum.zip(sender_iterated_stats, receiver_iterated_stats)

          case stage do
            :disable_low ->
              assert(
                Enum.any?(receiver_iterated_stats, &(&1["framesPerSecond"] == 0)),
                "Failed on stage: #{stage} should be all encodings disabled,
                receiver stats are #{inspect(receiver_iterated_stats)}
                sender stats are #{inspect(sender_iterated_stats)}
                "
              )

            _other ->
              assert(
                Enum.any?(stats, fn {all_encodings_sender_stats, receiver_stats} ->
                  stats_for_encoding?(receiver_stats, all_encodings_sender_stats, encoding)
                end),
                "Failed on stage: #{stage} should be encoding: #{encoding},
                    receiver stats are: #{inspect(receiver_iterated_stats)}
                    sender stats are: #{inspect(Enum.map(sender_iterated_stats, & &1[encoding]))}
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

  defp stats_for_encoding?(receiver_stats, all_encoding_sender_stats, encoding) do
    sender_stats = all_encoding_sender_stats[encoding]

    correct_dimensions? =
      receiver_stats["height"] == sender_stats["height"] and
        receiver_stats["width"] == sender_stats["width"]

    (correct_dimensions? or sender_stats["qualityLimitationReason"] != "none") and
      receiver_stats["callbackEncoding"] == encoding
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
