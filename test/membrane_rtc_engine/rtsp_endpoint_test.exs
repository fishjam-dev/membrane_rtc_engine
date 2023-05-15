defmodule Membrane.RTC.RTSPEndpointTest do
  use ExUnit.Case

  alias Membrane.RTC.Engine
  alias Membrane.RTC.Engine.Endpoint.{HLS, RTSP}
  alias Membrane.RTC.Engine.Message

  alias Membrane.HTTPAdaptiveStream.Manifest.SegmentDuration

  setup do
    options = [
      id: "test_rtc"
    ]

    {:ok, pid} = Engine.start_link(options, [])

    Engine.register(pid, self())

    on_exit(fn ->
      Engine.terminate(pid, blocking?: true)
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
  @fixture_profile "42c02a"
  @fixture_sps <<103, 66, 192, 42, 217, 0, 120, 2, 39, 229, 154, 129, 1, 2, 160, 0, 0, 3, 0, 32,
                 0, 0, 15, 1, 227, 6, 73>>
  @fixture_pps <<104, 203, 131, 203, 32>>

  test "invalid URI", %{rtc_engine: rtc_engine} do
    rtsp_endpoint = %RTSP{
      rtc_engine: rtc_engine,
      source_uri: "NOT_AN_URI"
    }

    :ok = Engine.add_endpoint(rtc_engine, rtsp_endpoint, endpoint_id: @rtsp_endpoint_id)

    assert_receive(%Message.EndpointCrashed{endpoint_id: @rtsp_endpoint_id}, 20_000)
    refute_received(_any)
  end

  test "reconnects", %{rtc_engine: rtc_engine} do
    reconnects = 3

    rtsp_endpoint = %RTSP{
      rtc_engine: rtc_engine,
      source_uri: "rtsp://asoifsafnwaefnajskfsv.co.uk:554/no/stream/here",
      max_reconnect_attempts: reconnects,
      reconnect_delay: 100
    }

    :ok = Engine.add_endpoint(rtc_engine, rtsp_endpoint, endpoint_id: @rtsp_endpoint_id)

    # First attempt to connect
    assert_receive(
      %Message.EndpointMessage{
        endpoint_id: @rtsp_endpoint_id,
        message: {:connection_failed, :nxdomain}
      },
      20_000
    )

    # Reconnect attempts
    assert_exactly_n_reconnects(reconnects, :nxdomain, 20_000)
    :ok = RTSP.request_reconnect(rtc_engine, @rtsp_endpoint_id)
    assert_exactly_n_reconnects(reconnects, :nxdomain, 20_000)

    :ok = Engine.remove_endpoint(rtc_engine, @rtsp_endpoint_id)
    refute_received(_any)
  end

  defp assert_exactly_n_reconnects(n, reason, timeout) do
    for _i <- 1..n do
      assert_receive(
        %Message.EndpointMessage{
          endpoint_id: @rtsp_endpoint_id,
          message: {:connection_failed, ^reason}
        },
        timeout
      )
    end

    assert_receive(%Message.EndpointMessage{
      endpoint_id: @rtsp_endpoint_id,
      message: :max_reconnects
    })
  end

  test "RTSP signalling and disconnects (with a fake server)", %{rtc_engine: rtc_engine} do
    self_pid = self()

    server_pid =
      spawn_link(fn ->
        FakeRTSPserver.start(@loopback_ip, @fake_server_port, @rtp_port, self_pid)
      end)

    assert_receive(:fake_server_ready, 20_000)

    rtsp_endpoint = %RTSP{
      rtc_engine: rtc_engine,
      source_uri: "rtsp://#{@loopback_ip}:#{@fake_server_port}/stream",
      rtp_port: @rtp_port,
      reconnect_delay: 500,
      keep_alive_interval: 500
    }

    :ok = Engine.add_endpoint(rtc_engine, rtsp_endpoint, endpoint_id: @rtsp_endpoint_id)

    assert_receive(
      %Message.EndpointMessage{
        endpoint_id: @rtsp_endpoint_id,
        message: :rtsp_setup_complete
      },
      20_000
    )

    # Fake server stops responding to GET_PARAMETER keep-alives after RTSP setup completes.
    # This simulates a connection loss, and should result in the endpoint shutting down
    assert_receive(
      %Message.EndpointMessage{
        endpoint_id: @rtsp_endpoint_id,
        message: :disconnected
      },
      20_000
    )

    assert_receive(%Message.EndpointCrashed{endpoint_id: @rtsp_endpoint_id}, 20_000)

    refute_received(_any)

    Process.exit(server_pid, :shutdown)
  end

  @tag :tmp_dir
  test "RTSP -> HLS conversion, single H264 input", %{rtc_engine: rtc_engine, tmp_dir: tmp_dir} do
    self_pid = self()

    server_pid =
      spawn_link(fn ->
        FakeRTSPserver.start(@loopback_ip, @fake_server_port, @rtp_port, self_pid, %{
          fixture_path: Path.join(@fixtures_dir, @fixture_filename),
          framerate: @fixture_framerate,
          profile: @fixture_profile,
          sps: @fixture_sps,
          pps: @fixture_pps
        })
      end)

    assert_receive(:fake_server_ready, 20_000)

    hls_endpoint = %HLS{
      rtc_engine: rtc_engine,
      owner: self(),
      output_directory: tmp_dir,
      synchronize_tracks?: false,
      hls_config: %HLS.HLSConfig{
        mode: :vod,
        target_window_duration: :infinity,
        segment_duration: %SegmentDuration{
          min: Membrane.Time.seconds(2),
          target: Membrane.Time.seconds(2)
        }
      }
    }

    :ok = Engine.add_endpoint(rtc_engine, hls_endpoint, endpoint_id: @hls_endpoint_id)

    rtsp_endpoint = %RTSP{
      rtc_engine: rtc_engine,
      source_uri: "rtsp://#{@loopback_ip}:#{@fake_server_port}/stream",
      rtp_port: @rtp_port,
      reconnect_delay: 500,
      keep_alive_interval: 999_999,
      pierce_nat: false
    }

    :ok = Engine.add_endpoint(rtc_engine, rtsp_endpoint, endpoint_id: @rtsp_endpoint_id)

    assert_receive(
      %Message.EndpointMessage{
        endpoint_id: @rtsp_endpoint_id,
        message: :rtsp_setup_complete
      },
      20_000
    )

    assert_receive(
      %Message.EndpointMessage{
        endpoint_id: @rtsp_endpoint_id,
        message: :new_rtp_stream
      },
      20_000
    )

    assert_receive({:playlist_playable, :video, output_dir}, 20_000)
    assert [stream_id | _empty] = File.ls!(tmp_dir)
    assert output_dir == Path.join(tmp_dir, stream_id)

    spawn(fn -> check_presence_of_output_files(output_dir, 4, 1000, self_pid) end)
    assert_receive(:output_files_present, 20_000)

    refute_received(_any)

    Process.exit(server_pid, :shutdown)
  end

  defp check_presence_of_output_files(dir, n_segment_files, retry_after, notify_pid) do
    segments = File.ls!(dir) |> Enum.filter(fn x -> x =~ ~r/^video_segment_[0-9]+_.*\.m4s/ end)

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
