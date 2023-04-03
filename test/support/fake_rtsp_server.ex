defmodule FakeRTSPserver do
  @moduledoc false

  @spec start(String.t(), pos_integer(), pos_integer(), pid()) :: any()
  def start(ip, port, client_port, parent_pid) do
    {:ok, socket} =
      :gen_tcp.listen(port, [:binary, packet: :line, active: false, reuseaddr: true])

    send(parent_pid, :fake_server_ready)
    loop_acceptor(socket, ip, port, client_port)
  end

  defp loop_acceptor(socket, ip, port, client_port) do
    {:ok, client} = :gen_tcp.accept(socket)

    serve(client, %{ip: ip, port: port, client_port: client_port, cseq: 0, server_state: :preinit})

    loop_acceptor(socket, ip, port, client_port)
  end

  defp serve(socket, state) when state.server_state == :preinit do
    request_type = do_serve(socket, state, [:describe, :get_parameter], [:setup, :play])
    new_state = if request_type == :describe, do: :init, else: :preinit
    serve(socket, %{state | cseq: state.cseq + 1, server_state: new_state})
  end

  defp serve(socket, state) when state.server_state == :init do
    request_type = do_serve(socket, state, [:describe, :get_parameter, :setup], [:play])
    new_state = if request_type == :setup, do: :ready, else: :init
    serve(socket, %{state | cseq: state.cseq + 1, server_state: new_state})
  end

  defp serve(socket, state) when state.server_state == :ready do
    request_type = do_serve(socket, state, [:describe, :get_parameter, :setup, :play])
    new_state = if request_type == :play, do: :playing, else: :ready
    serve(socket, %{state | cseq: state.cseq + 1, server_state: new_state})
  end

  defp serve(socket, state) when state.server_state == :playing do
    # Don't respond to GET_PARAMETER keep-alives in playing state
    do_serve(socket, state, [:describe, :setup, :play])
    serve(socket, %{state | cseq: state.cseq + 1})
  end

  defp do_serve(socket, state, respond_on, raise_on \\ []) do
    request = get_request(socket)

    request_type =
      case request do
        "DESCRIBE " <> _rest -> :describe
        "SETUP " <> _rest -> :setup
        "PLAY " <> _rest -> :play
        "GET_PARAMETER " <> _rest -> :get_parameter
        _other -> raise("RTSP Endpoint sent unrecognised request: #{inspect(request)}")
      end

    response_body =
      if Enum.any?(respond_on, fn allowed_request -> request_type == allowed_request end) do
        generate_response(request_type, state)
      end

    if is_nil(response_body) do
      if Enum.any?(raise_on, fn erroneous_request -> request_type == erroneous_request end) do
        raise("""
        RTSP Endpoint sent invalid request: #{inspect(request)}
        to fake server in state: #{inspect(state.server_state)}
        """)
      end
    else
      :gen_tcp.send(socket, "RTSP/1.0 200 OK\r\nCSeq: #{state.cseq}\r\n" <> response_body)
    end

    request_type
  end

  defp get_request(socket, request \\ "") do
    case :gen_tcp.recv(socket, 0) do
      {:ok, packet} ->
        request = request <> packet
        if packet != "\r\n", do: get_request(socket, request), else: request

      {:error, :closed} ->
        exit(:normal)

      {:error, reason} ->
        raise("Error when getting request: #{inspect(reason)}")
    end
  end

  defp generate_response(request_type, state) do
    case request_type do
      :describe ->
        sdp =
          "v=0\r\nm=video 0 RTP/AVP 96\r\na=control:rtsp://#{state.ip}:#{state.port}/control\r\n" <>
            "a=rtpmap:96 H264/90000\r\na=fmtp:96 profile-level-id=64001f; packetization-mode=1; " <>
            "sprop-parameter-sets=Z2QAH62EAQwgCGEAQwgCGEAQwgCEO1AoAt03AQEBQAAA+gAAOpgh,aO4xshs=\r\n"

        "Content-Base: rtsp://#{state.ip}:#{state.port}/stream\r\n" <>
          "Content-Type: application/sdp\r\nContent-Length: #{byte_size(sdp)}\r\n\r\n" <> sdp

      :setup ->
        "Transport: RTP/AVP;unicast;client_port=#{state.client_port};" <>
          "server_port=23456;ssrc=10443EAB\r\n\r\n"

      _play_or_get_parameter ->
        "\r\n"
    end
  end
end
