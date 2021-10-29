defmodule TestVideoroom.Integration.ClientTest do
  use TestVideoroomWeb.ConnCase, async: false
  use Wallaby.Feature

  import Wallaby.Query

  @room_name {:global, "room"}

  @page Routes.page_path(TestVideoroomWeb.Endpoint, :index)
  @start_button button("start")
  @stop_button button("stop")
  @stats_button button("stats")

  @data Query.css("#data")

  def message(msg), do: css(".messages > .message", text: msg)

  setup_all do
    {:ok, _pid} = TestVideoroom.Room.start(name: @room_name)

    :ok
  end

  setup do
    TestVideoroom.Room.register_new_peer_listener(@room_name, self())
  end

  @n 5


  @join_interval 4_000
  @leave_interval 5_000


  @sessions @n
  @moduletag timeout: 180_000
  feature "Users gradually joining and leaving can see each other", %{sessions: sessions} do
    user_entries = Enum.zip(sessions, List.duplicate(1, length(sessions)))

    # assert that users can see each other when others are gradually joining
    user_entries = Enum.reduce(user_entries, [], fn
     {new_user, _version} = user_entry, existing_users ->
        # join the room
        join_room(new_user)

        # wait till pipeline confirms peer's entry
        assert_receive {:room, :new_peer}, 5_000

        # wait for the frontend to initialize properly
        wait(@join_interval)

        # check that existing users can see the newly joined user (and everybody already being in the room)
        assert {:ok, updated_versions} = assert_user_see_others(existing_users, length(existing_users))

        # update existing users' versions
        existing_users =
          existing_users
          |> Enum.map(fn {user, _old_version} -> user end)
          |> Enum.zip(updated_versions)

        [user_entry | existing_users]
    end)

    # let the room work for a while
    wait(10_000)

    # assert that users can see each other when others are gradually leaving
    Enum.reduce(user_entries, user_entries, fn
     {user, _version} = user_entry, [_current_user_entry | existing_users] ->
        # leave the room
        leave_room(user)

        # wait for the frontend to stabilizes so other peers can see the user leave
        wait(@leave_interval)

        # existing user should see all other users, therefore -1
        assert {:ok, updated_versions} = assert_user_see_others(existing_users, length(existing_users) - 1)

        # update existing users' versions
        existing_users
        |> Enum.map(fn {user, _old_version} -> user end)
        |> Enum.zip(updated_versions)
    end)


    # make sure that the room is still alive
    assert :global.whereis_name("room") |> Process.alive?()
  end


  @sessions 5
  @moduletag timeout: 180_000
  feature "Users joining all at once can see each other", %{sessions: users} do
    # make all the users join the room
    for user <- users do
      join_room(user)
    end

    # wait for everybody to load
    wait(10_000)

    total_users = length(users)

    # check that users can see each other
    assert{:ok, _versions} =
      users
      |> Enum.zip(List.duplicate(1, total_users))
      |> assert_user_see_others(total_users - 1)


    # make sure that the room is still alive
    assert :global.whereis_name("room") |> Process.alive?()
  end

  defp join_room(session) do
    session
    |> visit(@page)
    |> click(@start_button)
  end

  defp leave_room(session) do
    session
    |> click(@stop_button)
  end

  defp assert_user_see_others(users_with_versions, expected_users) do
    versions = for {user, version} <- users_with_versions do
      assert {:ok, version} = assert_remotes_streams_playing(user, version, expected_users)

      version
    end

    {:ok, versions}
  end

  defp assert_remotes_streams_playing(session, expected_data_version, expected_streams) do
    assert {streams, new_expected_data_version} = fetch_remote_media_stream_stats(session, expected_data_version)

    assert length(streams) == expected_streams

    for stream <- streams do
      assert is_stream_playing(stream)
    end

    {:ok, new_expected_data_version}
  end

  defp fetch_remote_media_stream_stats(session, expected_version) do
    element =
      session
      |> click(@stats_button)
      |> find(@data)

    # make sure that the version tag has been updated
    find(session, data("version", to_string(expected_version)))

    element
    |> Wallaby.Element.text()
    |> case do
      "uninitialized" -> {:error, :uninitialized}
      data -> {Jason.decode!(data), expected_version + 1}
    end
  end

  defp is_stream_playing(%{"streamId" => _, "isAudioPlaying" => audio, "isVideoPlaying" => video}), do: audio and video

  defp wait(milliseconds), do: :timer.sleep(milliseconds)
end
