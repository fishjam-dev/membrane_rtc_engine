defmodule Membrane.RTC.RTSPEndpointTest do
  use ExUnit.Case

  alias Membrane.RTC.Engine
  alias Membrane.RTC.Engine.Endpoint.RTSP
  alias Membrane.RTC.Engine.Message

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
end
