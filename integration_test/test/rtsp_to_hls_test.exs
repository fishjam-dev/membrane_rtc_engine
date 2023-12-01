defmodule Membrane.RTC.Engine.Integration.RTSPtoHLStest do
  use ExUnit.Case

  alias Membrane.RTC.Engine
  alias Membrane.RTC.Engine.Endpoint.{HLS, RTSP}
  alias Membrane.RTC.Engine.Message

  setup do
    options = [
      id: "test_rtc"
    ]

    {:ok, pid} = Engine.start_link(options, [])

    Engine.register(pid, self())

    on_exit(fn ->
      Engine.terminate(pid)
    end)

    [rtc_engine: pid]
  end

  @rtsp_endpoint_id "rtsp-endpoint"
  @rtp_port 23_232
  @loopback_ip "127.0.0.1"
  @fake_server_port 554

  @hls_endpoint_id "hls-endpoint"
  @fixtures_dir "./test/fixtures/"

  @fixture_filename "video_baseline.h264"
  @fixture_framerate {60, 1}
  @fixture_hls_segments 4

  # At the moment, this test might not work with H264 profiles with B-frames
  # (any except :baseline and :constrained_baseline).
  # For more info refer to the issue available here:
  # https://membraneframework.atlassian.net/browse/MS-553
  @fixture_profile "42c02a"
  @fixture_sps <<103, 66, 192, 42, 217, 0, 120, 2, 39, 229, 154, 129, 1, 2, 160, 0, 0, 3, 0, 32,
                 0, 0, 15, 1, 227, 6, 73>>
  @fixture_pps <<104, 203, 131, 203, 32>>

  @tag :tmp_dir
  test "RTSP -> HLS conversion, single H264 input", %{rtc_engine: rtc_engine, tmp_dir: tmp_dir} do
    self_pid = self()

    start_link_supervised!(
      {FakeRTSPserver,
       ip: @loopback_ip,
       port: @fake_server_port,
       client_port: @rtp_port,
       parent_pid: self_pid,
       stream_ctx: %{
         fixture_path: Path.join(@fixtures_dir, @fixture_filename),
         framerate: @fixture_framerate,
         profile: @fixture_profile,
         sps: @fixture_sps,
         pps: @fixture_pps
       }},
      restart: :temporary
    )

    assert_receive(:fake_server_ready, 20_000)

    hls_endpoint = %HLS{
      rtc_engine: rtc_engine,
      owner: self(),
      output_directory: tmp_dir,
      synchronize_tracks?: false,
      hls_config: %HLS.HLSConfig{
        mode: :vod,
        target_window_duration: :infinity,
        segment_duration: Membrane.Time.seconds(2)
      }
    }

    :ok = Engine.add_endpoint(rtc_engine, hls_endpoint, id: @hls_endpoint_id)

    rtsp_endpoint = %RTSP{
      rtc_engine: rtc_engine,
      source_uri: "rtsp://#{@loopback_ip}:#{@fake_server_port}/stream",
      rtp_port: @rtp_port,
      pierce_nat: false
    }

    :ok = Engine.add_endpoint(rtc_engine, rtsp_endpoint, id: @rtsp_endpoint_id)

    assert_receive(
      %Message.EndpointMessage{
        endpoint_id: @rtsp_endpoint_id,
        message: :rtsp_setup_complete
      },
      2_000
    )

    assert_receive(
      %Message.EndpointMessage{
        endpoint_id: @rtsp_endpoint_id,
        message: :new_rtp_stream
      },
      2_000
    )

    assert_receive({:playlist_playable, :video, output_dir}, 10_000)

    spawn(fn ->
      check_presence_of_output_files(output_dir, @fixture_hls_segments, 1000, self_pid)
    end)

    assert_receive(:output_files_present, 20_000)
  end

  defp check_presence_of_output_files(dir, n_segment_files, retry_after, notify_pid) do
    segments = File.ls!(dir) |> Enum.filter(fn x -> x =~ ~r/^video_segment_[0-9]+_.*\.m4s$/ end)

    if length(segments) == n_segment_files and
         Enum.all?(segments, fn x ->
           Path.join(dir, x) |> File.stat!() |> Map.get(:size) > 0
         end) do
      send(notify_pid, :output_files_present)
    else
      Process.sleep(retry_after)
      check_presence_of_output_files(dir, n_segment_files, retry_after, notify_pid)
    end
  end
end
