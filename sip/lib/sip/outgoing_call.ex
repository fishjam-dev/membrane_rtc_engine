defmodule Membrane.RTC.Engine.Endpoint.SIP.OutgoingCall do
  @moduledoc false

  use Membrane.RTC.Engine.Endpoint.SIP.Call

  alias Membrane.RTC.Engine.Endpoint.SIP.{Call, SippetCore}
  alias Membrane.RTC.Engine.Endpoint.SIP.Call.SDP

  @death_timeout_ms 5000

  ## CALL MANAGEMENT API

  @spec cancel(Call.id()) :: :ok
  def cancel(call_id) do
    GenServer.cast(Call.registry_id(call_id), :cancel)
  end

  @spec bye(Call.id()) :: :ok
  def bye(call_id) do
    GenServer.cast(Call.registry_id(call_id), :bye)
  end

  ## SIP.Call CALLBACKS

  @impl Call
  def after_init(state) do
    proposal = SDP.proposal(state.external_ip, state.rtp_port)

    headers =
      Call.build_headers(:invite, state)
      |> Map.replace(:content_length, byte_size(proposal))

    message =
      Sippet.Message.build_request(:invite, to_string(state.callee))
      |> Map.put(:headers, headers)
      |> Sippet.Message.put_header(:content_type, "application/sdp")
      |> Map.replace(:body, proposal)

    Call.make_request(message, state)
  end

  @impl Call
  def handle_response(:invite, provisional, _response, state) when provisional in 100..199 do
    case provisional do
      100 -> notify_endpoint(state.endpoint, :trying)
      180 -> notify_endpoint(state.endpoint, :ringing)
      # Session Progress
      183 -> nil
      _xx -> Logger.warning("SIP Client: Unknown provisional response #{provisional}. Ignoring")
    end

    state
  end

  @impl Call
  def handle_response(:invite, 200, response, state) do
    state = send_ack(response, state)

    case SDP.parse(response.body) do
      {:ok, connection_info} ->
        notify_endpoint(state.endpoint, {:call_ready, connection_info})
        state

      {:error, reason} ->
        _state = build_and_send_request(:bye, state)

        # Give Sippet time to send the request (this is async)
        Process.sleep(20)

        raise "SIP Client: Received SDP answer is not matching our requirements: #{inspect(reason)}"
    end
  end

  @impl Call
  def handle_response(:invite, transfer, response, state) when transfer in [300, 301, 302] do
    case Sippet.Message.get_header(response, :contact, []) do
      [{"Transfer", uri, _map} | _] ->
        after_init(%{state | callee: uri})

      _other ->
        raise "SIP Client: Received #{transfer} response, but were unable to parse transfer URI"
    end
  end

  @impl Call
  def handle_response(:invite, declined, _response, state) when declined in [403, 603] do
    notify_endpoint(state.endpoint, {:end, :declined})
    state
  end

  @impl Call
  def handle_response(:invite, busy, _response, state) when busy in [486, 600] do
    notify_endpoint(state.endpoint, {:end, :busy})
    state
  end

  @impl Call
  def handle_response(:invite, 487, _response, state) do
    # Request Terminated -- this is the case when the INVITE was cancelled
    # by a separate CANCEL request before receiving a final response
    state
  end

  @impl Call
  def handle_response(:invite, 204, _response, _state) do
    # 204 is a success response, but we need SDP from the response body to setup the session
    # This shouldn't ever happen, but if we don't handle it here it would get silently ignored
    raise "SIP Client: Received 204 No Content in response to INVITE"
  end

  @impl Call
  def handle_response(method, 200, _response, state) when method in [:cancel, :bye] do
    send(self(), :die)
    state
  end

  @impl Call
  def handle_response(_method, status_code, response, state) do
    Call.handle_generic_response(status_code, response, state)
  end

  @impl Call
  def handle_request(:notify, request, state) do
    respond(request, 200)
    state
  end

  @impl Call
  def handle_request(:bye, request, state) do
    respond(request, 200)
    notify_endpoint(state.endpoint, {:end, hangup_cause(request)})
    Process.send_after(self(), :die, @death_timeout_ms)
    state
  end

  ## GenServer CALLBACKS

  @impl GenServer
  def handle_cast(:cancel, state) do
    state = build_and_send_request(:cancel, state)
    notify_endpoint(state.endpoint, {:end, :cancelled})
    Process.send_after(self(), :die, @death_timeout_ms)

    {:noreply, state}
  end

  @impl GenServer
  def handle_cast(:bye, state) do
    state = build_and_send_request(:bye, state)
    notify_endpoint(state.endpoint, {:end, :user_hangup})
    Process.send_after(self(), :die, @death_timeout_ms)

    {:noreply, state}
  end

  @impl GenServer
  def handle_info(:die, state) do
    {:stop, :normal, state}
  end

  ## PRIVATE FUNCTIONS

  defp build_and_send_request(method, state) do
    headers = Call.build_headers(method, state)

    message =
      Sippet.Message.build_request(method, to_string(state.callee))
      |> Map.put(:headers, headers)

    Call.make_request(message, state)
  end

  defp respond(request, code) do
    request
    |> Sippet.Message.to_response(code)
    |> SippetCore.send_message()
  end

  defp notify_endpoint(endpoint, message) do
    send(endpoint, {:call_info, message})
  end

  defp send_ack(response, state) when is_map_key(response.headers, :record_route) do
    %Sippet.Message{
      headers: %{
        to: to,
        via: [{_, _, _, %{"branch" => branch}}],
        cseq: {cseq, _method}
      }
    } = response

    route = Enum.reverse(response.headers.record_route)

    loose_routing? =
      route
      |> hd()
      |> elem(1)
      |> Map.get(:parameters)
      |> String.split(";")
      |> Enum.member?("lr")

    {state, route} =
      if loose_routing? do
        Logger.debug("Use loose routing")
        {%{state | callee: response.headers.contact |> hd() |> elem(1)}, route}
      else
        Logger.debug("Use strict routing")
        callee = route |> hd() |> elem(1) |> Map.put(:parameters, nil)

        {%{state | callee: callee}, Enum.drop(route, 1) ++ response.headers.contact}
      end

    headers =
      Call.build_headers(:ack, state, branch)
      |> Map.replace(:to, to)
      |> Map.replace(:cseq, {cseq, :ack})
      |> Map.put(:route, route)

    message =
      Sippet.Message.build_request(:ack, to_string(state.callee))
      |> Map.put(:headers, headers)

    message =
      if loose_routing? do
        target = route |> hd() |> elem(1)

        Map.put(message, :target, {:udp, target.host, target.port})
      else
        message
      end

    SippetCore.send_message(message)
    %{state | to: to, target: message.target, route: route}
  end

  defp send_ack(response, state) do
    %Sippet.Message{
      headers: %{
        to: to,
        via: [{_, _, _, %{"branch" => branch}}],
        cseq: {cseq, _method}
      }
    } = response

    state = %{state | callee: response.headers.contact |> hd() |> elem(1)}

    headers =
      Call.build_headers(:ack, state, branch)
      |> Map.replace(:to, to)
      |> Map.replace(:cseq, {cseq, :ack})

    message =
      Sippet.Message.build_request(:ack, to_string(state.callee))
      |> Map.put(:headers, headers)

    SippetCore.send_message(message)
    %{state | to: to}
  end

  defp hangup_cause(request) do
    case Sippet.Message.get_header(request, "X-Asterisk-HangupCause", []) do
      ["Normal Clearing"] -> :normal_clearing
      [] -> :unknown
      [other | _rest] -> other
    end
  end
end
