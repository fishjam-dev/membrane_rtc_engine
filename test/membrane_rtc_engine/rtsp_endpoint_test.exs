defmodule Membrane.RTC.RTSPEndpointTest do
  use ExUnit.Case

  alias Membrane.RTC.Engine
  alias Membrane.RTC.Engine.Endpoint.RTSP
  alias Membrane.RTC.Engine.Message

  defmodule FakeSignallingServer do
    @spec start(String.t(), pos_integer(), pos_integer()) :: any()
    def start(ip, port, client_port) do
      {:ok, socket} =
        :gen_tcp.listen(port, [:binary, packet: :line, active: false, reuseaddr: true])

      loop_acceptor(socket, ip, port, client_port)
    end

    defp loop_acceptor(socket, ip, port, client_port) do
      {:ok, client} = :gen_tcp.accept(socket)
      serve(client, %{ip: ip, port: port, client_port: client_port, cseq: 0})
      loop_acceptor(socket, ip, port, client_port)
    end

    defp serve(socket, state) do
      request = get_request(socket)

      response =
        case state.cseq do
          0 ->
            "DESCRIBE " <> _rest = request

            sdp =
              "v=0\r\nm=video 0 RTP/AVP 96\r\na=control:rtsp://#{state.ip}:#{state.port}/control\r\n" <>
                "a=rtpmap:96 H264/90000\r\na=fmtp:96 profile-level-id=64001f; packetization-mode=1; " <>
                "sprop-parameter-sets=Z2QAH62EAQwgCGEAQwgCGEAQwgCEO1AoAt03AQEBQAAA+gAAOpgh,aO4xshs=\r\n"

            "RTSP/1.0 200 OK\r\nCSeq: 0\r\nContent-Base: rtsp://#{state.ip}:#{state.port}/stream\r\n" <>
              "Content-Type: application/sdp\r\nContent-Length: #{byte_size(sdp)}\r\n\r\n" <> sdp

          1 ->
            "SETUP " <> _rest = request

            "RTSP/1.0 200 OK\r\nCSeq: 1\r\nTransport: RTP/AVP;unicast;client_port=#{state.client_port};" <>
              "server_port=23456;ssrc=10443EAB\r\n\r\n"

          2 ->
            "PLAY " <> _rest = request
            "RTSP/1.0 200 OK\r\nCSeq: 2\r\n\r\n"

          # Simulate the server not responding to keep-alives
          _other ->
            nil
        end

      if not is_nil(response), do: :gen_tcp.send(socket, response)

      serve(socket, %{state | cseq: state.cseq + 1})
    end

    defp get_request(socket, request \\ "") do
      {:ok, packet} = :gen_tcp.recv(socket, 0)
      request = request <> packet

      if packet != "\r\n" do
        get_request(socket, request)
      else
        request
      end
    end
  end

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
  @fake_server_port 62_137

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
    {:ok, server_pid} =
      Task.start_link(fn ->
        FakeSignallingServer.start(@loopback_ip, @fake_server_port, @rtp_port)
      end)

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

    # Fake server is not responding to GET_PARAMETER keep-alives.
    # This simulates a connection loss, and should result in the endpoint shutting down
    assert_receive(
      %Message.EndpointMessage{
        endpoint_id: @rtsp_endpoint_id,
        message: :disconnected
      },
      20_000
    )

    assert_receive(%Message.EndpointCrashed{endpoint_id: @rtsp_endpoint_id}, 20_000)

    :ok = Engine.remove_endpoint(rtc_engine, @rtsp_endpoint_id)
    refute_received(_any)

    Process.exit(server_pid, :shutdown)
  end
end
