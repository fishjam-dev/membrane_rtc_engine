defmodule TestVideoroom.Integration.MetadataTest do
  use TestVideoroomWeb.ConnCase, async: false

  import TestVideoroom.Integration.Utils

  # in miliseconds
  @peer_delay 500
  # in miliseconds
  @peer_duration 60_000
  @room_url "http://localhost:4001"

  # in miliseconds
  @join_interval 10_000

  @start_all "start-all"
  @update_peer "metadata-update-peer"
  @update_track "metadata-update-track"
  @metadata_peer "metadata-peer"
  @metadata_track "metadata-track"
  @browser_options %{count: 1, delay: @peer_delay, headless: true}

  @tag timeout: 180_000
  test "updating peer metadata works and updating track metadata works correctly" do
    browsers_number = 2

    pid = self()

    receiver = Process.spawn(fn -> receive_stats(browsers_number, pid) end, [:link])

    mustang_options = %{
      target_url: @room_url,
      linger: @peer_duration,
      join_interval: @join_interval,
      start_button: @start_all,
      receiver: receiver,
      actions: [],
      id: -1
    }

    actions1 = [
      {:click, @update_peer, 4_000},
      {:click, @update_track, 4_000}
    ]

    actions2 = [
      {:wait, 3_000},
      {:get_stats, @metadata_peer, 1, 1_000, tag: :peer_metadata},
      {:wait, 3_000},
      {:get_stats, @metadata_track, 1, 1_000, tag: :track_metadata}
    ]

    stage_to_text = %{
      peer_metadata: "test",
      track_metadata: "trackMetadata"
    }

    actions_with_id = [actions1, actions2] |> Enum.with_index()

    for {actions, browser_id} <- actions_with_id, into: [] do
      specific_mustang = %{mustang_options | id: browser_id, actions: actions}

      Task.async(fn ->
        Stampede.start({TestMustang, specific_mustang}, @browser_options)
      end)
    end
    |> Task.await_many(:infinity)

    receive do
      stats ->
        for {stage, browsers} <- stats do
          text = stage_to_text[stage]

          Enum.all?(browsers, fn
            {1, stats} ->
              assert(
                Enum.any?(stats, &(&1 == text)),
                "Failed on stage: #{stage} should be metadata: #{text}, but stats are #{inspect(stats)}"
              )

            {_other, _stats} ->
              true
          end)
        end
    end
  end
end
