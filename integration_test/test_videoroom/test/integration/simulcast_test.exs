defmodule TestVideoroom.Integration.SimulcastTest do
  use TestVideoroomWeb.ConnCase, async: false

  import TestVideoroom.Integration.Utils

  # in miliseconds
  @peer_delay 500
  # in miliseconds
  @peer_duration 60_000
  @room_url "http://localhost:4001"

  # in miliseconds
  @join_interval 30_000

  @start_with_simulcast "start-simulcast"
  @change_own_low "simulcast-local-low-encoding"
  @change_own_medium "simulcast-local-medium-encoding"
  @change_own_high "simulcast-local-high-encoding"
  @set_peer_encoding_low "simulcast-peer-low-encoding"
  @set_peer_encoding_medium "simulcast-peer-medium-encoding"
  @simulcast_inbound_stats "simulcast-inbound-stats"
  @simulcast_outbound_stats "simulcast-outbound-stats"
  @browser_options %{count: 1, delay: @peer_delay, headless: true}
  @test_duration 600_000

  @stats_number 10
  @stats_interval 1_000
  @keyframe_request_time 2_000
  # time needed to recognize variant as active
  @variant_activity_time 10_000
  @probe_times %{low_to_medium: 15_000, low_to_high: 30_000, nil_to_high: 50_000}

  @tag timeout: @test_duration
  test "disabling and enabling medium encoding again works correctly" do
    # wait 30 seconds
    # check if target was reached
    # send target for 1 minute
    # assert that we are receiving target for 1 minute
    # turn target off
    # assert we switched to lower encoding
    # wait for 1 minute
    # turn target on
    # assert we switched to target
    # wait for 1 minute
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

    sender_actions = [
      {:get_stats, @simulcast_inbound_stats, @stats_number, @stats_interval, tag: :after_warmup},
      {:click, @change_own_medium, @keyframe_request_time},
      {:get_stats, @simulcast_outbound_stats, @stats_number, @stats_interval,
       tag: :after_disabling_medium_en},
      {:click, @change_own_medium,
       @variant_activity_time + @probe_times[:low_to_medium] + @keyframe_request_time},
      {:get_stats, @simulcast_outbound_stats, @stats_number, @stats_interval,
       tag: :after_enabling_medium_en}
    ]

    receiver_actions = [
      {:get_stats, @simulcast_inbound_stats, @stats_number, @stats_interval, tag: :after_warmup},
      {:wait, @keyframe_request_time},
      {:get_stats, @simulcast_inbound_stats, @stats_number, @stats_interval,
       tag: :after_disabling_medium_en},
      {:wait, @variant_activity_time + @probe_times[:low_to_medium] + @keyframe_request_time},
      {:get_stats, @simulcast_inbound_stats, @stats_number, @stats_interval,
       tag: :after_enabling_medium_en}
    ]

    actions_with_id = [sender_actions, receiver_actions] |> Enum.with_index()

    tag_to_expected_encoding = %{
      after_warmup: "m",
      after_disabling_medium_en: "l",
      after_enabling_medium_en: "m"
    }

    for {actions, browser_id} <- actions_with_id, into: [] do
      specific_mustang = %{
        mustang_options
        | id: browser_id,
          actions: actions
      }

      Task.async(fn ->
        Stampede.start({TestMustang, specific_mustang}, @browser_options)
      end)
    end
    |> Task.await_many(:infinity)

    receive do
      stats ->
        for tag <- Map.keys(tag_to_expected_encoding),
            browser_id_to_stats_samples = Map.get(stats, tag) do
          encoding = tag_to_expected_encoding[tag]

          sender_stats_samples = browser_id_to_stats_samples[0]
          receiver_stats_samples = browser_id_to_stats_samples[1]

          assert_sender_receiver_stats(
            tag,
            encoding,
            sender_stats_samples,
            receiver_stats_samples
          )
        end
    end
  end

  @tag timeout: @test_duration
  test "changing encoding to low and then returning to medium works correctly " do
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

    receiver_actions = [
      {:get_stats, @simulcast_inbound_stats, @stats_number, @stats_interval, tag: :after_warmup},
      {:click, @set_peer_encoding_low, @keyframe_request_time},
      {:get_stats, @simulcast_inbound_stats, @stats_number, @stats_interval,
       tag: :after_switching_to_low_en},
      {:click, @set_peer_encoding_medium, @probe_times[:low_to_medium] + @keyframe_request_time},
      {:get_stats, @simulcast_inbound_stats, @stats_number, @stats_interval,
       tag: :after_switching_to_medium_en}
    ]

    sender_actions = [
      {:get_stats, @simulcast_inbound_stats, @stats_number, @stats_interval, tag: :after_warmup},
      {:wait, @keyframe_request_time},
      {:get_stats, @simulcast_outbound_stats, @stats_number, @stats_interval,
       tag: :after_switching_to_low_en},
      {:wait, @probe_times[:low_to_medium] + @keyframe_request_time},
      {:get_stats, @simulcast_outbound_stats, @stats_number, @stats_interval,
       tag: :after_switching_to_medium_en}
    ]

    actions_with_id = [receiver_actions, sender_actions] |> Enum.with_index()

    tag_to_expected_encoding = %{
      after_warmup: "m",
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
        Stampede.start({TestMustang, specific_mustang}, @browser_options)
      end)
    end
    |> Task.await_many(:infinity)

    receive do
      stats ->
        for tag <- Map.keys(tag_to_expected_encoding),
            browser_id_to_stats_samples = Map.get(stats, tag) do
          encoding = tag_to_expected_encoding[tag]

          sender_stats_samples = browser_id_to_stats_samples[1]
          receiver_stats_samples = browser_id_to_stats_samples[0]

          assert_sender_receiver_stats(
            tag,
            encoding,
            sender_stats_samples,
            receiver_stats_samples
          )
        end
    end
  end

  @tag timeout: @test_duration
  test "disabling gradually all encodings and then gradually enabling them works correctly" do
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

    sender_actions = [
      {:get_stats, @simulcast_inbound_stats, @stats_number, @stats_interval, tag: :after_warmup},
      {:click, @change_own_medium, @keyframe_request_time},
      {:get_stats, @simulcast_outbound_stats, @stats_number, @stats_interval,
       tag: :after_disabling_medium_en},
      {:click, @change_own_low, @probe_times[:low_to_high] + @keyframe_request_time},
      {:get_stats, @simulcast_outbound_stats, @stats_number, @stats_interval,
       tag: :after_disabling_low_en},
      {:click, @change_own_high, 1_000},
      {:get_stats, @simulcast_outbound_stats, @stats_number, @stats_interval,
       tag: :after_disabling_high_en},
      {:click, @change_own_high,
       @variant_activity_time + @probe_times[:nil_to_high] + @keyframe_request_time},
      {:get_stats, @simulcast_outbound_stats, @stats_number, @stats_interval,
       tag: :after_enabling_high_en},
      {:click, @change_own_low, @variant_activity_time + @keyframe_request_time},
      {:get_stats, @simulcast_outbound_stats, @stats_number, @stats_interval,
       tag: :after_enabling_low_en},
      {:click, @change_own_medium,
       @variant_activity_time + @probe_times[:low_to_medium] + @keyframe_request_time},
      {:get_stats, @simulcast_outbound_stats, @stats_number, @stats_interval,
       tag: :after_enabling_medium_en}
    ]

    receiver_actions = [
      {:get_stats, @simulcast_inbound_stats, @stats_number, @stats_interval, tag: :after_warmup},
      {:wait, @keyframe_request_time},
      {:get_stats, @simulcast_inbound_stats, @stats_number, @stats_interval,
       tag: :after_disabling_medium_en},
      {:wait, @probe_times[:low_to_high] + @keyframe_request_time},
      {:get_stats, @simulcast_inbound_stats, @stats_number, @stats_interval,
       tag: :after_disabling_low_en},
      {:wait, 1_000},
      {:get_stats, @simulcast_inbound_stats, @stats_number, @stats_interval,
       tag: :after_disabling_high_en},
      {:wait, @variant_activity_time + @probe_times[:nil_to_high] + @keyframe_request_time},
      {:get_stats, @simulcast_inbound_stats, @stats_number, @stats_interval,
       tag: :after_enabling_high_en},
      {:wait, @variant_activity_time + @keyframe_request_time},
      {:get_stats, @simulcast_inbound_stats, @stats_number, @stats_interval,
       tag: :after_enabling_low_en},
      {:wait, @variant_activity_time + @probe_times[:low_to_medium] + @keyframe_request_time},
      {:get_stats, @simulcast_inbound_stats, @stats_number, @stats_interval,
       tag: :after_enabling_medium_en}
    ]

    actions_with_id = [sender_actions, receiver_actions] |> Enum.with_index()

    tag_to_expected_encoding = %{
      after_warmup: "m",
      after_disabling_medium_en: "l",
      after_disabling_low_en: "h",
      after_disabling_high_en: nil,
      after_enabling_high_en: "h",
      after_enabling_low_en: "l",
      after_enabling_medium_en: "m"
    }

    for {actions, browser_id} <- actions_with_id, into: [] do
      specific_mustang = %{
        mustang_options
        | id: browser_id,
          actions: actions
      }

      Task.async(fn ->
        Stampede.start({TestMustang, specific_mustang}, @browser_options)
      end)
    end
    |> Task.await_many(:infinity)

    receive do
      stats ->
        for tag <- Map.keys(tag_to_expected_encoding),
            browser_id_to_stats_samples = Map.get(stats, tag) do
          encoding = tag_to_expected_encoding[tag]

          sender_stats_samples = browser_id_to_stats_samples[0]
          receiver_stats_samples = browser_id_to_stats_samples[1]

          case tag do
            :after_disabling_high_en ->
              assert(
                Enum.any?(receiver_stats_samples, &(&1["framesPerSecond"] == 0)),
                "Failed on tag: #{tag} should be all encodings disabled,
                receiver stats are #{inspect(receiver_stats_samples)}
                sender stats are #{inspect(sender_stats_samples)}
                "
              )

            _other ->
              assert_sender_receiver_stats(
                tag,
                encoding,
                sender_stats_samples,
                receiver_stats_samples
              )
          end
        end
    end
  end

  defp assert_receiver_encoding(receiver_stats, encoding) do
    receiver_stats["encoding"] == encoding
  end

  defp assert_stats_equal(receiver_stats, sender_encoding_stats) do
    # According to WebRTC standard, receiver is never aware of simulcast. Sender sends multiple encodings to SFU,
    # but SFU is switching between them transparently, forwarding always only one encoding to the receiver
    correct_dimensions? =
      receiver_stats["height"] == sender_encoding_stats["height"] and
        receiver_stats["width"] == sender_encoding_stats["width"]

    correct_dimensions? or sender_encoding_stats["qualityLimitationReason"] != "none"
  end

  defp assert_sender_receiver_stats(
         tag,
         receiver_encoding,
         sender_stats_samples,
         receiver_stats_samples
       ) do
    sender_stats_samples
    |> Enum.zip(receiver_stats_samples)
    |> Enum.all?(fn {sender_stats, receiver_stats} ->
      assert_stats_equal(receiver_stats, sender_stats[receiver_encoding]) and
        assert_receiver_encoding(receiver_stats, receiver_encoding)
    end)
    |> assert("Failed on tag: #{tag} should be encoding: #{receiver_encoding},
          receiver stats are: #{inspect(receiver_stats_samples, limit: :infinity, pretty: true)}
          sender stats are: #{inspect(Enum.map(sender_stats_samples, & &1[receiver_encoding]), limit: :infinity, pretty: true)}
          ")
  end
end
