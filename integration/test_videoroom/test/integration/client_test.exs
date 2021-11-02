defmodule TestVideoroom.Integration.ClientTest do
  use TestVideoroomWeb.ConnCase, async: false
  use Wallaby.Feature

  import Wallaby.Query

  require Logger

  @chrome_capabilities [
    capabilities: %{
      browserName: "chrome",
      javascriptEnabled: true,
      loadImages: true,
      version: "",
      rotatable: false,
      takesScreenshot: true,
      cssSelectorsEnabled: true,
      nativeEvents: false,
      platform: "ANY",
      unhandledPromptBehavior: "accept",
      loggingPrefs: %{
        browser: "DEBUG"
      },
      chromeOptions: %{
        args: [
          "--headless",
          "--use-fake-ui-for-media-stream",
          "--use-fake-device-for-media-stream"
        ]
      }
    }
  ]

  @firefox_capabilities [
    capabilities: %{
      browserName: "firefox",
      javascriptEnabled: true,
      loadImages: true,
      version: "",
      rotatable: false,
      takesScreenshot: true,
      cssSelectorsEnabled: true,
      nativeEvents: false,
      platform: "ANY",
      unhandledPromptBehavior: "accept",
      loggingPrefs: %{
        browser: "DEBUG"
      },
      # firefox_binary: "/Applications/Firefox.app/Contents/MacOS/firefox",
      "moz:firefoxOptions": %{
        args: ["--headless"],
        prefs: %{
          "permissions.default.microphone" => true,
          "permissions.default.camera" => true,
          "media.navigator.streams.fake" => true,
          "general.useragent.override" =>
            "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"
        }
      }
    }
  ]

  @room_name {:global, "room"}

  @page Routes.page_path(TestVideoroomWeb.Endpoint, :index)

  @start_with_all_button button("start-all")
  @start_with_mic_button button("start-mic-only")
  @start_with_camera_button button("start-camera-only")
  @start_with_nothing_button button("start-none")

  @stop_button button("stop")
  @stats_button button("stats")

  @data Query.css("#data")

  setup_all do

    # start 3 chrome and 3 firefox sessions for each test to
    # avoid high initialization time for each test
    sessions =
      1..3
      |> Enum.flat_map(fn _ ->
        [{:firefox, @firefox_capabilities}, {:chrome, @chrome_capabilities}]
      end)
      |> Enum.map(fn {browser, capabilities} ->
        {:ok, session} = Wallaby.start_session(capabilities)

        {browser, session}
      end)
      |> Enum.group_by(& elem(&1, 0), & elem(&1, 1))


    on_exit(fn ->
      Map.values(sessions) |> List.flatten() |> Enum.each(& Wallaby.end_session(&1))
    end)

    {:ok, browser_sessions: sessions}
  end

  def sessions_for_browser(%{browser_sessions: sessions}, browser, n) when n <= 3 and browser in [:chrome, :firefox] do
    Enum.take(sessions[browser], n)
  end

  setup ctx do
    {:ok, _pid} = TestVideoroom.Room.start(name: @room_name)
    TestVideoroom.Room.register_new_peer_listener(@room_name, self())

    on_exit(fn ->
      :ok = GenServer.stop(:global.whereis_name("room"))

      ctx.browser_sessions
      |> Map.values()
      |> List.flatten()
      |> Enum.each(fn session ->
        visit(session, @page)
      end)

    end)
  end

  @join_interval 5_000
  @leave_interval 5_000

  @moduletag timeout: 180_000
  @sessions 0
  feature "Users gradually joining and leaving can hear and see each other", ctx do
    user_entries = prepare_users(sessions_for_browser(ctx, :chrome, 3) ++ sessions_for_browser(ctx, :firefox, 2))

    # assert that users can see each other when others are gradually joining
    user_entries =
      Enum.reduce(user_entries, [], fn
        {new_user, _version} = user_entry, existing_users ->
          # join the room
          join_room(new_user)

          # wait till pipeline confirms peer's entry
          assert_receive {:room, :new_peer}, 5_000

          # wait for the frontend to initialize properly
          wait(@join_interval)
          Logger.info("User joined")

          # check that existing users can see the newly joined user (and everybody already being in the room)
          assert {:ok, updated_versions} =
                   assert_user_see_others(existing_users, length(existing_users))

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
      {user, _version}, [_current_user_entry | existing_users] ->
        # leave the room
        leave_room(user)

        # wait for the frontend to stabilizes so other peers can see the user leave
        wait(@leave_interval)
        Logger.info("User left")

        # existing user should see all other users, therefore -1
        assert {:ok, updated_versions} =
                 assert_user_see_others(existing_users, length(existing_users) - 1)

        # update existing users' versions
        existing_users
        |> Enum.map(fn {user, _old_version} -> user end)
        |> Enum.zip(updated_versions)
    end)

    # make sure that the room is still alive
    assert :global.whereis_name("room") |> Process.alive?()
  end

  @moduletag timeout: 180_000
  @sessions 0
  feature "Users joining all at once can hear and see each other", ctx do
    user_entries = prepare_users(sessions_for_browser(ctx, :chrome, 3) ++ sessions_for_browser(ctx, :firefox, 3))

    # make all the users join the room
    for {user, _version} <- user_entries do
      join_room(user)
    end

    # wait for everybody to load
    wait(15_000)

    total_users = length(user_entries)

    # check that users can see each other
    assert {:ok, _versions} = assert_user_see_others(user_entries, total_users - 1)

    # make all users leave the room to check if it did not crash
    for {user, _versino} <- user_entries do
      leave_room(user)
    end

    # wait for all people to leave
    wait(10_000)

    # make sure that the room is still alive
    assert :global.whereis_name("room") |> Process.alive?()
  end

  @moduletag timeout: 180_000
  @sessions 0
  feature "Users joining without either microphone, camera or both can see or hear other users", ctx do
    user_entries = prepare_users(sessions_for_browser(ctx, :chrome, 2) ++ sessions_for_browser(ctx, :firefox, 2))

    # join all users
    user_entries
    |> Enum.zip([:all, :mic, :camera, :none])
    |> Enum.each(fn {{user, _version}, type} ->
      join_room(user, type)

      assert_receive {:room, :new_peer}, 5_000
      wait(@join_interval)
    end)

    # wait for everybody to load
    wait(10_000)

    [user_with_all, user_with_mic, user_with_camera, user_with_none] = user_entries

    # The below scenarios check if users can accordingly see or hear each other if they enable media
    # NOTE: user without any media is not visible to anyone in a WebRTC way

    # User with 'all' media should see:
    # * see a user with camera only
    # * hear a user with microphone only

    {stats, _version} = fetch_remote_media_stream_stats(user_with_all)
    assert length(stats) == 2

    # can hear the user with mic only
    assert Enum.any?(stats, &is_stream_playing(&1, %{audio: true, video: false}))

    # can see the user with camera only
    assert Enum.any?(stats, &is_stream_playing(&1, %{audio: false, video: true}))

    # User with 'mic' only should see:
    # * see and hear a user with all media
    # * see a user with camera only

    {stats, _version} = fetch_remote_media_stream_stats(user_with_mic)
    assert length(stats) == 2

    # can hear and see the user with all media
    assert Enum.any?(stats, &is_stream_playing(&1, %{audio: true, video: true}))

    # can see the user with camera only
    assert Enum.any?(stats, &is_stream_playing(&1, %{audio: false, video: true}))

    # User with 'camera' only should see:
    # * see and hear a user with all media
    # * hear a user with mic only

    {stats, _version} = fetch_remote_media_stream_stats(user_with_camera)
    assert length(stats) == 2

    # can hear and see the user with all media
    assert Enum.any?(stats, &is_stream_playing(&1, %{audio: true, video: true}))

    # can hear the user with mic only
    assert Enum.any?(stats, &is_stream_playing(&1, %{audio: true, video: false}))

    # User with 'none' media should:
    # * see and hear a user with all media
    # * hear a user with mic only
    # * see a user with camera only

    {stats, _version} = fetch_remote_media_stream_stats(user_with_none)
    assert length(stats) == 3

    # can hear and see the user with all media
    assert Enum.any?(stats, &is_stream_playing(&1, %{audio: true, video: true}))

    # can hear the user with mic only
    assert Enum.any?(stats, &is_stream_playing(&1, %{audio: true, video: false}))

    # can see the user with camera only
    assert Enum.any?(stats, &is_stream_playing(&1, %{audio: false, video: true}))


    Enum.each(user_entries, fn {user, _version} ->
      leave_room(user)
    end)

    # make sure that the room is still alive
    assert :global.whereis_name("room") |> Process.alive?()
  end

  defp prepare_users(sessions) do
    sessions
    |> Enum.zip(List.duplicate(1, length(sessions)))
  end

  defp join_room(session, type \\ :all) do
    button =
      case type do
        :all -> @start_with_all_button
        :mic -> @start_with_mic_button
        :camera -> @start_with_camera_button
        :none -> @start_with_nothing_button
      end

    session
    |> visit(@page)
    |> click(button)
  end

  defp leave_room(session) do
    session
    |> click(@stop_button)
  end

  # asserts that each user can see and hear all the other users
  defp assert_user_see_others(users_with_versions, expected_users) do
    versions =
      for {user, version} <- users_with_versions do
        assert {:ok, version} = assert_remotes_streams_playing(user, version, expected_users)

        version
      end

    {:ok, versions}
  end

  # asserts that all remote streams are both playing audio and video
  defp assert_remotes_streams_playing(session, expected_data_version, expected_streams) do
    assert {streams, new_expected_data_version} =
             fetch_remote_media_stream_stats({session, expected_data_version})

    assert length(streams) == expected_streams

    for stream <- streams do
      assert is_stream_playing(stream),
             "Session [#{inspect(session.capabilities.browserName)} -> #{inspect(session.id)}] expected a stream to be playing, got #{inspect stream}"
    end

    {:ok, new_expected_data_version}
  end

  defp fetch_remote_media_stream_stats({session, expected_version}) do
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

  defp is_stream_playing(stats, expected \\ %{audio: true, video: true})

  defp is_stream_playing(
         %{"streamId" => _, "isAudioPlaying" => audio, "isVideoPlaying" => video},
         %{audio: expected_audio, video: expected_video}
       ),
       do: audio == expected_audio and video == expected_video

  defp wait(milliseconds), do: :timer.sleep(milliseconds)
end
