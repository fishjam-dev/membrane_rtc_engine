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
      actions: [],
      simulcast_inbound_stats_button: @simulcast_inbound_stats,
      simulcast_outbound_stats_button: @simulcast_outbound_stats,
      id: -1
    }

    actions1 = [
      {:click, @change_own_medium, 1, 1_000, tag: nil},
      {:click, @simulcast_outbound_stats, 10, 1_000, tag: :after_disabling_medium_en},
      {:click, @change_own_medium, 1, 1_000, tag: nil},
      {:click, @simulcast_outbound_stats, 10, 1_000, tag: :after_enabling_medium_en}
    ]

    actions2 = [
      {:wait, 1_000},
      {:click, @simulcast_inbound_stats, 10, 1_000, tag: :after_disabling_medium_en},
      {:wait, 1_000},
      {:click, @simulcast_inbound_stats, 10, 1_000, tag: :after_enabling_medium_en}
    ]

    actions_with_id = [actions1, actions2] |> Enum.with_index()

    stage_to_expected_encoding = %{after_disabling_medium_en: "h", after_enabling_medium_en: "m"}

    for {actions, browser_id} <- actions_with_id, into: [] do
      specific_mustang = %{
        mustang_options
        | id: browser_id,
          actions: actions
      }

      Task.async(fn ->
        Stampede.start({SimulcastMustang, specific_mustang}, @browser_options)
      end)
    end
    |> Task.await_many(:infinity)

    receive do
      accumulated_stats ->
        for stage <- Map.keys(stage_to_expected_encoding),
            browser_id_to_iterated_stats = Map.get(accumulated_stats, stage) do
          encoding = stage_to_expected_encoding[stage]

          sender_iterated_stats = browser_id_to_iterated_stats[0]
          receiver_iterated_stats = browser_id_to_iterated_stats[1]
          stats = Enum.zip(sender_iterated_stats, receiver_iterated_stats)

          assert(
            Enum.any?(stats, fn {all_encodings_sender_stats, receiver_stats} ->
              assert_stats_equal(receiver_stats, all_encodings_sender_stats[encoding]) and
                assert_receiver_encoding(receiver_stats, encoding)
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
      actions: [],
      simulcast_inbound_stats_button: @simulcast_inbound_stats,
      simulcast_outbound_stats_button: @simulcast_outbound_stats,
      id: -1
    }

    actions1 = [
      {:click, @set_peer_encoding_low, 1, 1_000, tag: nil},
      {:click, @simulcast_inbound_stats, 10, 1_000, tag: :after_switching_to_low_en},
      {:click, @set_peer_encoding_medium, 1, 1_000, tag: nil},
      {:click, @simulcast_inbound_stats, 10, 1_000, tag: :after_switching_to_medium_en}
    ]

    actions2 = [
      {:wait, 1_000},
      {:click, @simulcast_outbound_stats, 10, 1_000, tag: :after_switching_to_low_en},
      {:wait, 1_000},
      {:click, @simulcast_outbound_stats, 10, 1_000, tag: :after_switching_to_medium_en}
    ]

    actions_with_id = [actions1, actions2] |> Enum.with_index()

    stage_to_expected_encoding = %{
      after_switching_to_low_en: "l",
      after_switching_to_medium_en: "m"
    }

    for {actions, browser_id} <- actions_with_id, into: [] do
      specific_mustang = %{
        mustang_options
        | id: browser_id,
          actions: actions
      }

      Task.async(fn ->
        Stampede.start({SimulcastMustang, specific_mustang}, @browser_options)
      end)
    end
    |> Task.await_many(:infinity)

    receive do
      accumulated_stats ->
        for stage <- Map.keys(stage_to_expected_encoding),
            browser_id_to_iterated_stats = Map.get(accumulated_stats, stage) do
          encoding = stage_to_expected_encoding[stage]

          sender_iterated_stats = browser_id_to_iterated_stats[1]
          receiver_iterated_stats = browser_id_to_iterated_stats[0]
          stats = Enum.zip(sender_iterated_stats, receiver_iterated_stats)

          assert(
            Enum.any?(stats, fn {all_encodings_sender_stats, receiver_stats} ->
              assert_stats_equal(receiver_stats, all_encodings_sender_stats[encoding]) and
                assert_receiver_encoding(receiver_stats, encoding)
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
      actions: [],
      simulcast_inbound_stats_button: @simulcast_inbound_stats,
      simulcast_outbound_stats_button: @simulcast_outbound_stats,
      id: -1
    }

    actions1 = [
      {:click, @change_own_medium, 1, 1_000, tag: nil},
      {:click, @simulcast_outbound_stats, 10, 1_000, tag: :after_disabling_medium_en},
      {:click, @change_own_high, 1, 1_000, tag: nil},
      {:click, @simulcast_outbound_stats, 10, 1_000, tag: :after_disabling_high_en},
      {:click, @change_own_low, 1, 1_000, tag: nil},
      {:click, @simulcast_outbound_stats, 10, 1_000, tag: :after_disabling_low_en},
      {:click, @change_own_low, 1, 1_000, tag: nil},
      {:click, @simulcast_outbound_stats, 10, 1_000, tag: :after_enabling_low_en},
      {:click, @change_own_high, 1, 1_000, tag: nil},
      {:click, @simulcast_outbound_stats, 10, 1_000, tag: :after_enabling_high_en},
      {:click, @change_own_medium, 1, 1_000, tag: nil},
      {:click, @simulcast_outbound_stats, 10, 1_000, tag: :after_enabling_medium_en}
    ]

    actions2 = [
      {:wait, 1_000},
      {:click, @simulcast_inbound_stats, 10, 1_000, tag: :after_disabling_medium_en},
      {:wait, 1_000},
      {:click, @simulcast_inbound_stats, 10, 1_000, tag: :after_disabling_high_en},
      {:wait, 1_000},
      {:click, @simulcast_inbound_stats, 10, 1_000, tag: :after_disabling_low_en},
      {:wait, 1_000},
      {:click, @simulcast_inbound_stats, 10, 1_000, tag: :after_enabling_low_en},
      {:wait, 1_000},
      {:click, @simulcast_inbound_stats, 10, 1_000, tag: :after_enabling_high_en},
      {:wait, 1_000},
      {:click, @simulcast_outbound_stats, 10, 1_000, tag: :after_enabling_medium_en}
    ]

    actions_with_id = [actions1, actions2] |> Enum.with_index()

    stage_to_expected_encoding = %{
      after_disabling_medium_en: "h",
      after_disabling_high_en: "l",
      after_disabling_low_en: nil,
      after_enabling_low_en: "l",
      after_enabling_high_en: "h",
      after_enabling_medium_en: "m"
    }

    for {actions, browser_id} <- actions_with_id, into: [] do
      specific_mustang = %{
        mustang_options
        | id: browser_id,
          actions: actions
      }

      Task.async(fn ->
        Stampede.start({SimulcastMustang, specific_mustang}, @browser_options)
      end)
    end
    |> Task.await_many(:infinity)

    receive do
      accumulated_stats ->
        for stage <- Map.keys(stage_to_expected_encoding),
            browser_id_to_iterated_stats = Map.get(accumulated_stats, stage) do
          encoding = stage_to_expected_encoding[stage]

          sender_iterated_stats = browser_id_to_iterated_stats[0]
          receiver_iterated_stats = browser_id_to_iterated_stats[1]
          stats = Enum.zip(sender_iterated_stats, receiver_iterated_stats)

          case stage do
            :after_disabling_low_en ->
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
                  assert_stats_equal(receiver_stats, all_encodings_sender_stats[encoding]) and
                    assert_receiver_encoding(receiver_stats, encoding)
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

  defp assert_receiver_encoding(receiver_stats, encoding) do
    receiver_stats["encoding"] == encoding
  end

  defp assert_stats_equal(receiver_stats, sender_stats) do
    correct_dimensions? =
      receiver_stats["height"] == sender_stats["height"] and
        receiver_stats["width"] == sender_stats["width"]

    correct_dimensions? or sender_stats["qualityLimitationReason"] != "none"
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
