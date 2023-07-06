defmodule TestVideoroom.Integration.ContainerisedTest do
  use TestVideoroomWeb.ConnCase, async: false

  require Logger

  @moduletag :containerised

  # in milliseconds
  @max_test_duration 400_000

  # Name of shared volume folder
  @shared_folder "shared"

  # Necessary for peer ID mapping
  @sender_stats_tag :local_stream_stats

  # Keys of fields inside a single stat entry which we use
  @stat_entry_peer_id "peerId"
  @stat_entry_frames_received "framesReceived"

  @packet_loss_browser_id "browser0"
  @packet_loss_tags_to_check [:after_warmup, :after_applying_packet_loss_on_one_user]
  # How many result entries will be discarded when checking the ratio between frames
  # received from regular browsers and frames received from the one with packet loss applied
  @packet_loss_results_ratio_warmup 5
  # Max allowed difference between received frames from separate browsers, at each stat entry
  @max_frame_count_difference 10

  @tag timeout: @max_test_duration
  @tag :packet_loss_test
  test "packet loss on one browser" do
    TestVideoroom.Integration.ResultReceiver.start_link(browser_count: 3, parent: self())

    results = receive_loop()

    for {tag, tag_results} <- prepare_results(results) do
      assert_packet_loss_stats(tag, tag_results)
    end
  end

  defp receive_loop() do
    receive do
      {:results, results} ->
        results

      {:enable_packet_loss, name} ->
        Logger.info("#{inspect(name)} reports ready to enable packet loss")
        File.write!(Path.join(@shared_folder, "ENABLE_PACKET_LOSS"), <<>>)
        receive_loop()

      other ->
        raise("Received unexpected message: #{inspect(other)}")
    end
  end

  # We need to transform the results to be able to work with them more easily
  # The results we receive have the following structure:
  #   %{
  #     "browser0" => %{    # Everything we received from the browser "browser0"
  #       tag0: %{
  #         "browser0" => [     # "browser0" => All entries from this browser, related to this tag
  #                             # This layer is entirely redundant (there is only one entry in this map, "browser0")
  #           [                   # Entries from the two other browsers, from around the same time
  #             %{...},             # single stat entry from the first other browser
  #             ${...}              # single stat entry from the second other browser
  #           ],
  #           [                   # Entries from the two other browsers (another reading)
  #             ...
  #           ],
  #           ... # (as many entries as there have been readings for the given tag)
  #         ]
  #       },
  #       tag1: %{
  #         ...
  #       }
  #     },
  #     "browser1" => %{    # Everything we received from the browser "browser1"
  #       ...
  #     }
  #   }
  #
  # After calling `prepare_results/1`, the results will have the following structure:
  #   %{
  #     tag0: [
  #       %{                        # Entries from all browsers, from around the same time
  #         "browser0" => %{          # Browser-receiver (the one we read stats from) =>
  #           "browser1" => %{...},     # Browser-sender => single stat entry
  #           "browser2" => %{...}      # Browser-sender => single stat entry
  #         },
  #         "browser1" => %{          # as above
  #           "browser0" => %{...},
  #           "browser2" => %{...}
  #         }
  #       },
  #       %{                        # Entries from all browsers (another reading)
  #         ...
  #       },
  #       ... # (as many entries as there have been readings for the given tag)
  #     ],
  #     tag1: [
  #       ...
  #     ]
  #   }
  defp prepare_results(results) do
    peer_id_to_browser_id = get_peer_id_mapping(results)

    Map.new(@packet_loss_tags_to_check, fn tag ->
      initialised_acc =
        Enum.at(results, 0)
        |> then(fn {browser_id, browser_results} ->
          tag_results = browser_results[tag][browser_id]
          for _entry <- tag_results, do: %{}
        end)

      tag_list =
        Enum.reduce(results, initialised_acc, fn {browser_id, browser_results}, acc_list ->
          tag_results = browser_results[tag][browser_id]

          Enum.zip_with(acc_list, tag_results, fn acc_single, res ->
            Map.put(acc_single, browser_id, label_stat_entries(res, peer_id_to_browser_id))
          end)
        end)

      {tag, tag_list}
    end)
  end

  defp get_peer_id_mapping(results) do
    Map.new(results, fn {browser_id, browser_results} ->
      peer_id =
        browser_results[@sender_stats_tag][browser_id]
        |> hd()
        |> Map.get(@stat_entry_peer_id)

      {peer_id, browser_id}
    end)
  end

  defp label_stat_entries(stat_entries, peer_id_to_browser_id) do
    Map.new(stat_entries, fn entry ->
      browser_id = Map.get(peer_id_to_browser_id, entry[@stat_entry_peer_id])

      {browser_id, entry}
    end)
  end

  defp assert_packet_loss_stats(:after_warmup, tag_results) do
    frame_count_base = get_frame_count_base(tag_results)

    for result <- tag_results, {receiver_id, browser_result} <- result do
      # Assert that the browser received a similar amount of frames from every other browser
      {frames_min, frames_max} =
        Enum.map(browser_result, fn {sender_id, stat_entry} ->
          stat_entry[@stat_entry_frames_received] - frame_count_base[receiver_id][sender_id]
        end)
        |> Enum.min_max()

      assert frames_max - frames_min <= @max_frame_count_difference

      # Assert that every other browser received a similar amount of frames from this given
      # browser
      {frames_min, frames_max} =
        Map.delete(result, receiver_id)
        |> Enum.map(fn {other_receiver_id, other_browser_result} ->
          other_browser_result[receiver_id][@stat_entry_frames_received] -
            frame_count_base[other_receiver_id][receiver_id]
        end)
        |> Enum.min_max()

      assert frames_max - frames_min <= @max_frame_count_difference
    end
  end

  defp assert_packet_loss_stats(:after_applying_packet_loss_on_one_user, tag_results) do
    frame_count_base = get_frame_count_base(tag_results)

    for {result, result_idx} <- Enum.with_index(tag_results),
        # Assert only on browsers without packet loss
        result_filtered = Map.delete(result, @packet_loss_browser_id),
        {receiver_id, browser_result} <- result_filtered do
      # Assert that the browser received a similar amount of frames from every other browser
      # without packet loss
      {frames_min, frames_max} =
        Map.delete(browser_result, @packet_loss_browser_id)
        |> Enum.map(fn {sender_id, stat_entry} ->
          stat_entry[@stat_entry_frames_received] - frame_count_base[receiver_id][sender_id]
        end)
        |> Enum.min_max()

      assert frames_max - frames_min <= @max_frame_count_difference

      # After some time with packet loss applied, assert that the browser received
      # at least 1.5x less frames from the browser with packet loss
      if result_idx >= @packet_loss_results_ratio_warmup do
        packet_loss_frames =
          browser_result[@packet_loss_browser_id][@stat_entry_frames_received] -
            frame_count_base[receiver_id][@packet_loss_browser_id]

        frame_ratio = if packet_loss_frames == 0, do: 2, else: frames_max / packet_loss_frames

        assert frame_ratio >= 1.5
      end

      # Assert that every other browser without packet loss received a similar amount
      # of frames from this given browser
      {frames_min, frames_max} =
        Map.delete(result_filtered, receiver_id)
        |> Enum.map(fn {other_receiver_id, other_browser_result} ->
          other_browser_result[receiver_id][@stat_entry_frames_received] -
            frame_count_base[other_receiver_id][receiver_id]
        end)
        |> Enum.min_max()

      assert frames_max - frames_min <= @max_frame_count_difference
    end
  end

  defp get_frame_count_base(tag_results) do
    Map.new(hd(tag_results), fn {receiver_id, browser_result} ->
      frame_count_per_browser =
        Map.new(browser_result, fn {sender_id, stat_entry} ->
          {sender_id, stat_entry[@stat_entry_frames_received]}
        end)

      {receiver_id, frame_count_per_browser}
    end)
  end
end
