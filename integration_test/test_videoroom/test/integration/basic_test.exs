defmodule TestVideoroom.Integration.BasicTest do
  use TestVideoroomWeb.ConnCase, async: false

  import TestVideoroom.Integration.Utils

  @room_url "http://localhost:4001"

  # in miliseconds
  @warmup_time 6_000

  @start_with_all "start-all"
  @start_with_mic "start-mic-only"
  @start_with_camera "start-camera-only"
  @start_with_nothing "start-none"
  @stats "stats"
  @browser_options %{count: 1, headless: true}
  @actions [
    {:get_stats, @stats, 1, 0, tag: :after_warmup},
    {:wait, 60_000},
    {:get_stats, @stats, 1, 0, tag: :before_leave}
  ]

  @tag timeout: 180_000
  test "Users gradually joining and leaving can hear and see each other" do
    browsers_number = 4

    pid = self()

    receiver = Process.spawn(fn -> receive_stats(browsers_number, pid) end, [:link])

    mustang_options = %{
      target_url: @room_url,
      warmup_time: @warmup_time,
      start_button: @start_with_all,
      actions: @actions,
      receiver: receiver,
      id: -1
    }

    for browser <- 0..(browsers_number - 1), into: [] do
      mustang_options = %{mustang_options | id: browser}

      task =
        Task.async(fn ->
          Stampede.start({TestMustang, mustang_options}, @browser_options)
        end)

      Process.sleep(10_000)
      task
    end
    |> Task.await_many(:infinity)

    receive do
      acc ->
        for {stage, browsers} <- acc do
          case stage do
            :after_warmup ->
              Enum.all?(browsers, fn {browser_id, stats_list} ->
                Enum.each(stats_list, fn stats ->
                  assert length(stats) == browser_id
                  assert Enum.all?(stats, &is_stream_playing(&1))
                end)
              end)

            :before_leave ->
              Enum.all?(browsers, fn {browser_id, stats_list} ->
                Enum.each(stats_list, fn stats ->
                  assert length(stats) == browsers_number - browser_id - 1
                  assert Enum.all?(stats, &is_stream_playing(&1))
                end)
              end)
          end
        end
    end
  end

  @tag timeout: 180_000
  test "Users joining all at once can hear and see each other" do
    browsers_number = 4

    pid = self()

    receiver = Process.spawn(fn -> receive_stats(browsers_number, pid) end, [:link])

    mustang_options = %{
      target_url: @room_url,
      warmup_time: @warmup_time,
      start_button: @start_with_all,
      actions: @actions,
      receiver: receiver,
      id: -1
    }

    for browser <- 0..(browsers_number - 1), into: [] do
      mustang_options = %{mustang_options | id: browser}
      Process.sleep(500)

      Task.async(fn ->
        Stampede.start({TestMustang, mustang_options}, @browser_options)
      end)
    end
    |> Task.await_many(:infinity)

    receive do
      acc ->
        Enum.all?(acc, fn {stage, browsers} ->
          case stage do
            :after_warmup ->
              Enum.all?(browsers, fn {_browser_id, stats_list} ->
                Enum.each(stats_list, fn stats ->
                  assert length(stats) == browsers_number - 1
                  assert Enum.all?(stats, &is_stream_playing(&1))
                end)
              end)

            :before_leave ->
              true
          end
        end)
    end
  end

  @tag timeout: 180_000
  test "Users joining without either microphone, camera or both can see or hear other users" do
    browsers_number = 4

    pid = self()

    receiver = Process.spawn(fn -> receive_stats(browsers_number, pid) end, [:link])

    mustang_options = %{
      target_url: @room_url,
      warmup_time: @warmup_time,
      start_button: @start_with_all,
      actions: @actions,
      receiver: receiver,
      id: -1
    }

    buttons_with_id =
      [@start_with_all, @start_with_camera, @start_with_mic, @start_with_nothing]
      |> Enum.with_index()
      |> Map.new(fn {button, browser_id} -> {browser_id, button} end)

    for {browser_id, button} <- buttons_with_id, into: [] do
      specific_mustang = %{mustang_options | start_button: button, id: browser_id}

      Process.sleep(1000)

      Task.async(fn ->
        Stampede.start({TestMustang, specific_mustang}, @browser_options)
      end)
    end
    |> Task.await_many(:infinity)

    {_button, buttons_with_id} = Map.pop!(buttons_with_id, 3)

    receive do
      acc ->
        Enum.all?(acc, fn {stage, browsers} ->
          case stage do
            :after_warmup ->
              Enum.all?(browsers, fn {browser_id, stats_list} ->
                Enum.each(stats_list, fn stats ->
                  assert length(stats) == if(browser_id == 3, do: 3, else: 2)
                  {_value, new_buttons} = Map.pop(buttons_with_id, browser_id)
                  new_buttons = Map.values(new_buttons)
                  assert_streams_playing(stats, new_buttons)
                end)
              end)

            :before_leave ->
              true
          end
        end)
    end
  end

  defp assert_streams_playing(stats, buttons) do
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
  end

  defp is_stream_playing(stats, expected \\ %{audio: true, video: true})

  defp is_stream_playing(
         %{"streamId" => _, "isAudioPlaying" => audio, "isVideoPlaying" => video},
         %{audio: expected_audio, video: expected_video}
       ),
       do: audio == expected_audio and video == expected_video
end
