defmodule Membrane.RTC.Engine.Endpoint.SIP.OutgoingCall do
  @moduledoc false

  use Membrane.RTC.Engine.Endpoint.SIP.Call
  require Membrane.Logger

  alias Membrane.RTC.Engine.Endpoint.SIP.{Call, SippetCore}
  alias Membrane.RTC.Engine.Endpoint.SIP.Call.SDP

  @death_timeout_ms 5000

  @response_trying 100
  @response_ringing 180
  @response_session_progress 183
  @response_ok 200
  @response_no_content 204
  @response_multiple_choices 300
  @response_moved_permanently 301
  @response_moved_temporarily 302
  @response_forbidden 403
  @response_busy_here 486
  @response_request_terminated 487
  @response_busy_everywhere 600
  @response_decline 603

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

    :invite
    |> Call.build_headers(state, %{
      content_length: byte_size(proposal),
      content_type: "application/sdp"
    })
    |> create_request(state)
    |> Map.replace(:body, proposal)
    |> Call.make_request(state)
  end

  @impl Call
  def handle_response(:invite, provisional, _response, state) when provisional in 100..199 do
    case provisional do
      @response_trying ->
        notify_endpoint(state.endpoint, :trying)

      @response_ringing ->
        notify_endpoint(state.endpoint, :ringing)

      @response_session_progress ->
        nil

      _other ->
        Logger.warning("SIP Client: Unknown provisional response #{provisional}. Ignoring")
    end

    state
  end

  @impl Call
  def handle_response(:invite, @response_ok, response, state) do
    send_ack(response, state)

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
  def handle_response(:invite, transfer, _response, state)
      when transfer in [
             @response_multiple_choices,
             @response_moved_permanently,
             @response_moved_temporarily
           ] do
    # Callee was updated by `Call.process_response/2`.
    # We need to clear the `to` header, otherwise we will receive 481 Call/Transaction Does Not Exist
    after_init(%{state | to: nil})
  end

  @impl Call
  def handle_response(:invite, declined, _response, state)
      when declined in [@response_forbidden, @response_decline] do
    notify_endpoint(state.endpoint, {:end, :declined})
    state
  end

  @impl Call
  def handle_response(:invite, busy, _response, state)
      when busy in [@response_busy_here, @response_busy_everywhere] do
    notify_endpoint(state.endpoint, {:end, :busy})
    state
  end

  @impl Call
  def handle_response(:invite, @response_request_terminated, _response, state) do
    # Request Terminated -- this is the case when the INVITE was cancelled
    # by a separate CANCEL request before receiving a final response
    state
  end

  @impl Call
  def handle_response(:invite, @response_no_content, _response, _state) do
    # 204 is a success response, but we need SDP from the response body to setup the session
    # This shouldn't ever happen, but if we don't handle it here it would get silently ignored
    raise "SIP Client: Received 204 No Content in response to INVITE"
  end

  @impl Call
  def handle_response(method, @response_ok, _response, state) when method in [:cancel, :bye] do
    schedule_death(:instant)
    state
  end

  @impl Call
  def handle_response(_method, status_code, response, state) do
    Call.handle_generic_response(status_code, response, state)
  end

  @impl Call
  def handle_request(:notify, request, state) do
    respond(request, @response_ok)
    state
  end

  @impl Call
  def handle_request(:bye, request, state) do
    respond(request, @response_ok)
    notify_endpoint(state.endpoint, {:end, hangup_cause(request)})
    schedule_death()
    state
  end

  ## GenServer CALLBACKS

  @impl GenServer
  def handle_cast(:cancel, state) do
    state = send_cancel(state)
    notify_endpoint(state.endpoint, {:end, :cancelled})
    schedule_death()

    {:noreply, state}
  end

  @impl GenServer
  def handle_cast(:bye, state) do
    state = build_and_send_request(:bye, state)
    notify_endpoint(state.endpoint, {:end, :user_hangup})
    schedule_death()

    {:noreply, state}
  end

  @impl GenServer
  def handle_info(:die, state) do
    {:stop, :normal, state}
  end

  ## PRIVATE FUNCTIONS

  defp build_and_send_request(method, state) do
    method
    |> Call.build_headers(state)
    |> create_request(state)
    |> Call.make_request(state)
  end

  defp create_request(%{cseq: {_cseq, method}} = headers, state) do
    method
    |> Sippet.Message.build_request(to_string(state.callee))
    |> Map.put(:headers, headers)
  end

  defp respond(request, code) do
    request
    |> Sippet.Message.to_response(code)
    |> SippetCore.send_message()
  end

  defp notify_endpoint(endpoint, message) do
    send(endpoint, {:call_info, message})
  end

  defp send_ack(response, state) do
    %Sippet.Message{
      headers: %{
        to: to,
        via: [{_, _, _, %{"branch" => branch}}],
        cseq: {cseq, :invite}
      }
    } = response

    message =
      :ack
      |> Call.build_headers(state, %{to: to, cseq: {cseq, :ack}}, branch)
      |> create_request(state)

    message =
      if is_nil(state.target),
        do: message,
        else: Map.put(message, :target, state.target)

    SippetCore.send_message(message)
  end

  defp send_cancel(state) do
    headers =
      state.last_message.headers
      |> Map.drop([:content_length, :content_type, :proxy_authorization, :authorization])
      |> Map.update!(:cseq, fn {cseq, :invite} -> {cseq, :cancel} end)

    callee = %{state.registrar_credentials.uri | userinfo: state.phone_number}

    headers
    |> create_request(%{state | callee: callee})
    |> SippetCore.send_message()

    state
  end

  defp hangup_cause(request) do
    case Sippet.Message.get_header(request, "X-Asterisk-HangupCause", []) do
      ["Normal Clearing"] -> :normal_clearing
      [] -> :unknown
      [other | _rest] -> other
    end
  end

  defp schedule_death(), do: Process.send_after(self(), :die, @death_timeout_ms)
  defp schedule_death(:instant), do: send(self(), :die)
end
