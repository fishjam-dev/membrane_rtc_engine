defmodule TestVideoroom.Integration.MetadataTest do
  use TestVideoroomWeb.ConnCase, async: false

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

  @moduletag timeout: 180_000
  test "Check updatePeerMetadata and then updateTrackMetadata" do
    browsers_number = 2

    pid = self()

    receiver = Process.spawn(fn -> receive_stats(browsers_number, pid) end, [:link])

    mustang_options = %{
      target_url: @room_url,
      linger: @peer_duration,
      join_interval: @join_interval,
      start_button: @start_all,
      receiver: receiver,
      buttons: [],
      id: -1
    }

    timeouts = [4_000, 4_000]

    buttons1 = [@update_peer, @update_track] |> Enum.zip(timeouts)

    buttons2 = [@metadata_peer, @metadata_track] |> Enum.zip(timeouts)

    stages = [:peer_metadata, :track_metadata]

    create_room(mustang_options)

    buttons_with_id =
      [buttons1, buttons2]
      |> Enum.map(&Enum.zip(&1, stages))
      |> Enum.with_index()
      |> Map.new(fn {button, browser_id} -> {browser_id, button} end)

    for {browser_id, buttons} <- buttons_with_id, into: [] do
      specific_mustang = %{mustang_options | id: browser_id, buttons: buttons}

      Task.async(fn ->
        Stampede.start({SimulcastMustang, specific_mustang}, @browser_options)
      end)
    end
    |> Task.await_many(:infinity)

    receive do
      acc ->
        for {stage, browsers} <- acc do
          text =
            case stage do
              :peer_metadata -> "test"
              :track_metadata -> "trackMetadata"
            end

          Enum.all?(browsers, fn
            {1, stats} ->
              assert(
                stats == text,
                "Failed on stage: #{stage} should be metadata: #{text}, but stats are #{inspect(stats)}"
              )

            {_other, _stats} ->
              true
          end)
        end
    end
  end

  defp create_room(mustang_options) do
    # Creating room earlier to avoid error :already_started
    Task.async(fn ->
      Stampede.start({RoomMustang, mustang_options}, @browser_options)
    end)
    |> Task.await(:infinity)
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
