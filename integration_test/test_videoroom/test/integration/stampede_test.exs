defmodule TestVideoroom.Integration.ClientTest do
  use TestVideoroomWeb.ConnCase, async: false

  # in miliseconds
  @peer_delay 500
  # in miliseconds
  @peer_duration 60_000
  @room_url "http://localhost:4001"

  # in miliseconds
  @join_interval 6_000

  @start_with_all "start-all"
  @start_with_mic "start-mic-only"
  @start_with_camera "start-camera-only"
  @start_with_nothing "start-none"
  @browser_options %{count: 1, delay: @peer_delay, headless: true}

  @moduletag timeout: 180_000
  test "Users gradually joining and leaving can hear and see each other" do
    browsers_number = 4

    pid = self()

    receiver = Process.spawn(fn -> receive_stats(browsers_number, pid) end, [:link])

    mustang_options = %{
      target_url: @room_url,
      linger: @peer_duration,
      join_interval: @join_interval,
      start_button: @start_with_all,
      receiver: receiver,
      id: -1
    }

    for browser <- 0..(browsers_number - 1), into: [] do
      mustang_options = %{mustang_options | id: browser}

      task =
        Task.async(fn ->
          Stampede.start({IntegrationMustang, mustang_options}, @browser_options)
        end)

      Process.sleep(10_000)
      task
    end
    |> Task.await_many(:infinity)

    receive do
      acc ->
        Enum.all?(acc, fn {stage, browsers} ->
          case stage do
            :after_join ->
              Enum.all?(browsers, fn {browser_id, stats} ->
                assert length(stats) == browser_id
                assert Enum.all?(stats, &is_stream_playing(&1))
                true
              end)

            :before_leave ->
              Enum.all?(browsers, fn {browser_id, stats} ->
                assert length(stats) == browsers_number - browser_id - 1
                assert Enum.all?(stats, &is_stream_playing(&1))
              end)
          end
        end)
    end
  end

  @moduletag timeout: 180_000
  test "Users joining all at once can hear and see each other" do
    browsers_number = 4

    pid = self()

    receiver = Process.spawn(fn -> receive_stats(browsers_number, pid) end, [:link])

    mustang_options = %{
      target_url: @room_url,
      linger: @peer_duration,
      join_interval: @join_interval,
      start_button: @start_with_all,
      receiver: receiver,
      id: -1
    }

    # Creating room earlier to avoid error :already_started
    create_room(mustang_options)

    for browser <- 0..(browsers_number - 1), into: [] do
      mustang_options = %{mustang_options | id: browser}
      Process.sleep(500)

      Task.async(fn ->
        Stampede.start({IntegrationMustang, mustang_options}, @browser_options)
      end)
    end
    |> Task.await_many(:infinity)

    receive do
      acc ->
        Enum.all?(acc, fn {stage, browsers} ->
          case stage do
            :after_join ->
              Enum.all?(browsers, fn {_browser_id, stats} ->
                assert length(stats) == browsers_number - 1
                assert Enum.all?(stats, &is_stream_playing(&1))
              end)

            :before_leave ->
              true
          end
        end)
    end
  end

  @moduletag timeout: 180_000
  test "Users joining without either microphone, camera or both can see or hear other users" do
    browsers_number = 4

    pid = self()

    receiver = Process.spawn(fn -> receive_stats(browsers_number, pid) end, [:link])

    mustang_options = %{
      target_url: @room_url,
      linger: @peer_duration,
      join_interval: @join_interval,
      start_button: @start_with_all,
      receiver: receiver,
      id: -1
    }

    # Creating room earlier to avoid error :already_started
    create_room(mustang_options)

    buttons_with_id =
      [@start_with_all, @start_with_camera, @start_with_mic, @start_with_nothing]
      |> Enum.with_index()
      |> Map.new(fn {button, browser_id} -> {browser_id, button} end)

    for {browser_id, button} <- buttons_with_id, into: [] do
      specific_mustang = %{mustang_options | start_button: button, id: browser_id}

      Process.sleep(1000)

      Task.async(fn ->
        Stampede.start({IntegrationMustang, specific_mustang}, @browser_options)
      end)
    end
    |> Task.await_many(:infinity)

    {_button, buttons_with_id} = Map.pop!(buttons_with_id, 3)

    receive do
      acc ->
        Enum.all?(acc, fn {stage, browsers} ->
          case stage do
            :after_join ->
              Enum.all?(browsers, fn {browser_id, stats} ->
                assert length(stats) == if(browser_id == 3, do: 3, else: 2)
                {_value, new_buttons} = Map.pop(buttons_with_id, browser_id)
                new_buttons = Map.values(new_buttons)
                assert which_streams_playing(stats, new_buttons)
              end)

            :before_leave ->
              true
          end
        end)
    end
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

  defp which_streams_playing(stats, buttons) do
    for button <- buttons do
      case button do
        @start_with_all ->
          assert Enum.any?(stats, &is_stream_playing(&1))

        @start_with_camera ->
          assert Enum.any?(stats, &is_stream_playing(&1, %{audio: false, video: true}))

        @start_with_mic ->
          assert Enum.any?(stats, &is_stream_playing(&1, %{audio: true, video: false}))

        @start_with_nothing ->
          assert Enum.any?(stats, &is_stream_playing(&1, %{audio: false, video: false}))
      end
    end

    true
  end

  defp is_stream_playing(stats, expected \\ %{audio: true, video: true})

  defp is_stream_playing(
         %{"streamId" => _, "isAudioPlaying" => audio, "isVideoPlaying" => video},
         %{audio: expected_audio, video: expected_video}
       ),
       do: audio == expected_audio and video == expected_video

  defp create_room(mustang_options) do
    # Creating room earlier to avoid error :already_started
    Task.async(fn ->
      Stampede.start({RoomMustang, mustang_options}, @browser_options)
    end)
    |> Task.await(:infinity)
  end
end
