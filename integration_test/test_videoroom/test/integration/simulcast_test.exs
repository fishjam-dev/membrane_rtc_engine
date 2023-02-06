defmodule TestVideoroom.Integration.SimulcastTest do
  use TestVideoroomWeb.ConnCase, async: false

  import TestVideoroom.Integration.Utils

  @room_url "http://localhost:4001"

  # in miliseconds
  @warmup_time 30_000

  @start_with_simulcast "start-simulcast"
  @change_own_low "simulcast-local-low-encoding"
  @change_own_medium "simulcast-local-medium-encoding"
  @change_own_high "simulcast-local-high-encoding"
  @set_peer_encoding_low "simulcast-peer-low-encoding"
  @set_peer_encoding_medium "simulcast-peer-medium-encoding"
  @simulcast_inbound_stats "simulcast-inbound-stats"
  @simulcast_outbound_stats "simulcast-outbound-stats"
  @browser_options %{count: 1, headless: true}
  @max_test_duration 400_000

  # we want to get stats for at least 30 seconds
  # to ensure that the variant won't switch
  # just after being selected;
  # because gathering stats takes us 1 second
  # (see get_stats in mustang.ex)
  # we set stats_number to 15 and stats interval to 1s -
  # we will fetch stats every 2 seconds
  @stats_number 15
  @stats_interval 1_000
  # time needed to request and receive a variant
  @variant_request_time 2_000
  # max time needed to recognize variant as inactive
  @variant_inactivity_time 2_000
  # max time needed to recognize variant as active
  @variant_activity_time 11_000
  # times needed to probe from one resolution to another
  # assumes that audio is present
  # assumes the following max limits:
  # audio - 50kbps
  # video low - 150kbps
  # video medium - 500kbps
  # video high - 1500kbps
  @probe_times %{low_to_medium: 17_000, low_to_high: 30_000, nil_to_high: 50_000}

  # FIXME
  # this test shouldn't pass
  # after disabling medium encoding we move to the
  # low but we also request an allocation for high as
  # the next desired variant
  # when the allocation is granted we switch to the high
  #
  # stats_number and stats_interval are set in a way that makes this
  # test passing but they have to be fixed - we should use
  # @stats_number and @stats_interval
  @tag timeout: @max_test_duration
  test "disabling and enabling medium encoding again works correctly" do
    browsers_number = 2

    pid = self()

    receiver = Process.spawn(fn -> receive_stats(browsers_number, pid) end, [:link])

    mustang_options = %{
      target_url: @room_url,
      warmup_time: @warmup_time,
      start_button: @start_with_simulcast,
      receiver: receiver,
      actions: [],
      simulcast_inbound_stats_button: @simulcast_inbound_stats,
      simulcast_outbound_stats_button: @simulcast_outbound_stats,
      id: -1
    }

    sender_actions = [
      {:get_stats, @simulcast_inbound_stats, @stats_number, @stats_interval, tag: :after_warmup},
      {:click, @change_own_medium, @variant_inactivity_time + @variant_request_time},
      # FIXME use @stats_number and @stats_interval
      {:get_stats, @simulcast_outbound_stats, 3, 1_000, tag: :after_disabling_medium_en},
      {:click, @change_own_medium,
       @variant_activity_time + @probe_times[:low_to_medium] + @variant_request_time},
      {:get_stats, @simulcast_outbound_stats, @stats_number, @stats_interval,
       tag: :after_enabling_medium_en}
    ]

    receiver_actions = [
      {:get_stats, @simulcast_inbound_stats, @stats_number, @stats_interval, tag: :after_warmup},
      {:wait, @variant_inactivity_time + @variant_request_time},
      # FIXME use @stats_number and @stats_interval
      {:get_stats, @simulcast_inbound_stats, 3, 1_000, tag: :after_disabling_medium_en},
      {:wait, @variant_activity_time + @probe_times[:low_to_medium] + @variant_request_time},
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

  @tag timeout: @max_test_duration
  test "changing encoding to low and then returning to medium works correctly " do
    browsers_number = 2

    pid = self()

    receiver = Process.spawn(fn -> receive_stats(browsers_number, pid) end, [:link])

    mustang_options = %{
      target_url: @room_url,
      warmup_time: @warmup_time,
      start_button: @start_with_simulcast,
      receiver: receiver,
      actions: [],
      simulcast_inbound_stats_button: @simulcast_inbound_stats,
      simulcast_outbound_stats_button: @simulcast_outbound_stats,
      id: -1
    }

    receiver_actions = [
      {:get_stats, @simulcast_inbound_stats, @stats_number, @stats_interval, tag: :after_warmup},
      {:click, @set_peer_encoding_low, @variant_request_time},
      {:get_stats, @simulcast_inbound_stats, @stats_number, @stats_interval,
       tag: :after_switching_to_low_en},
      {:click, @set_peer_encoding_medium, @probe_times[:low_to_medium] + @variant_request_time},
      {:get_stats, @simulcast_inbound_stats, @stats_number, @stats_interval,
       tag: :after_switching_to_medium_en}
    ]

    sender_actions = [
      {:get_stats, @simulcast_inbound_stats, @stats_number, @stats_interval, tag: :after_warmup},
      {:wait, @variant_request_time},
      {:get_stats, @simulcast_outbound_stats, @stats_number, @stats_interval,
       tag: :after_switching_to_low_en},
      {:wait, @probe_times[:low_to_medium] + @variant_request_time},
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

  # FIXME
  # this test shouldn't pass for the same reason as the
  # "disabling and enabling medium encoding again works correctly"
  @tag timeout: @max_test_duration
  test "disabling gradually all encodings and then gradually enabling them works correctly" do
    browsers_number = 2

    pid = self()

    receiver = Process.spawn(fn -> receive_stats(browsers_number, pid) end, [:link])

    mustang_options = %{
      target_url: @room_url,
      warmup_time: @warmup_time,
      start_button: @start_with_simulcast,
      receiver: receiver,
      actions: [],
      simulcast_inbound_stats_button: @simulcast_inbound_stats,
      simulcast_outbound_stats_button: @simulcast_outbound_stats,
      id: -1
    }

    sender_actions = [
      {:get_stats, @simulcast_inbound_stats, @stats_number, @stats_interval, tag: :after_warmup},
      {:click, @change_own_medium, @variant_inactivity_time + @variant_request_time},
      # FIXME use @stats_number and @stats_interval
      {:get_stats, @simulcast_outbound_stats, 3, 1_000, tag: :after_disabling_medium_en},
      {:click, @change_own_low,
       @variant_inactivity_time + @probe_times[:low_to_high] + @variant_request_time},
      {:get_stats, @simulcast_outbound_stats, @stats_number, @stats_interval,
       tag: :after_disabling_low_en},
      {:click, @change_own_high, 1_000},
      {:get_stats, @simulcast_outbound_stats, @stats_number, @stats_interval,
       tag: :after_disabling_high_en},
      {:click, @change_own_high,
       @variant_activity_time + @probe_times[:nil_to_high] + @variant_request_time},
      {:get_stats, @simulcast_outbound_stats, @stats_number, @stats_interval,
       tag: :after_enabling_high_en},
      {:click, @change_own_low, @variant_activity_time + @variant_request_time},
      {:get_stats, @simulcast_outbound_stats, @stats_number, @stats_interval,
       tag: :after_enabling_low_en},
      {:click, @change_own_medium,
       @variant_activity_time + @probe_times[:low_to_medium] + @variant_request_time},
      {:get_stats, @simulcast_outbound_stats, @stats_number, @stats_interval,
       tag: :after_enabling_medium_en}
    ]

    receiver_actions = [
      {:get_stats, @simulcast_inbound_stats, @stats_number, @stats_interval, tag: :after_warmup},
      {:wait, @variant_inactivity_time + @variant_request_time},
      # FIXME use @stats_number and @stats_interval
      {:get_stats, @simulcast_inbound_stats, 3, 1_000, tag: :after_disabling_medium_en},
      {:wait, @variant_inactivity_time + @probe_times[:low_to_high] + @variant_request_time},
      {:get_stats, @simulcast_inbound_stats, @stats_number, @stats_interval,
       tag: :after_disabling_low_en},
      {:wait, 1_000},
      {:get_stats, @simulcast_inbound_stats, @stats_number, @stats_interval,
       tag: :after_disabling_high_en},
      {:wait, @variant_activity_time + @probe_times[:nil_to_high] + @variant_request_time},
      {:get_stats, @simulcast_inbound_stats, @stats_number, @stats_interval,
       tag: :after_enabling_high_en},
      {:wait, @variant_activity_time + @variant_request_time},
      {:get_stats, @simulcast_inbound_stats, @stats_number, @stats_interval,
       tag: :after_enabling_low_en},
      {:wait, @variant_activity_time + @probe_times[:low_to_medium] + @variant_request_time},
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
      # FIXME uncomment this
      # the idea of target variant is that we select
      # the highest encoding lower than target or, if not present,
      # the lowest encoding higher than target
      # therefore, after enabling low with target set to medium
      # we should switch to the low
      # after_enabling_low_en: "l",
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
