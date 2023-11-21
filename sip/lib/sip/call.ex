defmodule Membrane.RTC.Engine.Endpoint.SIP.Call do
  @moduledoc false
  use Bunch.Access

  require Membrane.Logger

  alias Membrane.RTC.Engine.Endpoint.SIP.SDP

  def start_link(client_id, registrar_credentials, sip_port, rtp_port) do
    call_id = Sippet.Message.create_call_id()
    {:ok, pid} = GenServer.start_link(__MODULE__, [call_id, client_id, registrar_credentials, sip_port, rtp_port], name: registry_id(call_id))
    {call_id, pid}
  end

  def register(call_id) do
    GenServer.cast(registry_id(call_id), :register)
    {:ok, call_id}
  end

  def dial(call_id, phone_number) do
    GenServer.cast(registry_id(call_id), {:invite, phone_number})
  end

  def bye(call_id) do
    GenServer.cast(registry_id(call_id), :bye)
  end

  def registry_id(call_id) do
    {:via, Registry, {SipEndpoint.CallRegistry, call_id}}
  end

  def exists?(call_id) do
    case Registry.lookup(SipEndpoint.CallRegistry, call_id) do
      [] -> false
      _  -> true
    end
  end

  def handle_response(call_id, response) do
    GenServer.cast(registry_id(call_id), {:handle_response, response})
  end

  def handle_notify(call_id, req) do
    GenServer.cast(registry_id(call_id), {:send, 200, req})
  end

  def handle_bye(call_id, req) do
    send(Client.registry_id(state.client_id), {:end, call_id})
    respond(call_id, 200, req)
    GenServer.stop(registry_id(call_id), :shutdown)
  end

  def respond(call_id, code, req) do
    GenServer.cast(registry_id(call_id), {:send, code, req})
  end

  defp send_ack(calee, headers_base, call_id, cseq)
    headers = %{ headers_base |
      call_id: call_id,
      method: :ack,
      cseq: cseq,
      content_length: 0
    } |> create_headers()

    message = Sippet.Message.build_request(:ack, to_string(calee))
      |> Map.put(:headers, headers)
    Sippet.send(client_id, message)
  end

  defp apply_digest_auth(response, state) do
    new_cseq = state.cseq
    {request, new_cseq} = new_request(state)
    {:ok, request} = Sippet.DigestAuth.make_request(request, response, &get_userinfo/1)
    Sippet.send(state.client_id, request)
    new_cseq
  end

  defp create_headers(headers_base) do
    %{
      max_forwards: headers_base.max_forwards,
      from: headers_base.from,
      to: headers_base.to,
      via: [put_elem(headers_base.via, 3, %{"branch" => Sippet.Message.create_branch()})],
      contact: headers_base.contact,
      user_agent: headers_base.user_agent,
      cseq: {headers_base.cseq, headers_base.method},
      call_id: headers_base.call_id,
      content_length: headers_base.content_length
    }
  end

  @impl true
  def init(call_id, client_id, %RegistrarCredentials{domain: domain, username: username} = registrar_credentials, sip_port, rtp_port) do
    
    from_address = Sippet.URI%{scheme: "SIP", userinfo: username, host: external_ip, port: sip_port},
    caller_identifier = "Jellyfish"
    tag = Sippet.Message.create_tag()
    user_agent = "Jellyfish/SIP/0.3"

    state = %{
      external_ip: ...,
      call_id: call_id,
      client_id: client_id,
      registrar_credentials: registrar_credentials,
      sip_port: sip_port,
      rtp_port: rtp_port,
      calee: nil,
      headers_base: %{
        from: {
          caller_identifier,
          from_address,
          %{"tag" => tag}     
        },
        via: {{2, 0}, :udp, {external_ip, sip_port}},
        contact: [{
          caller_identifier,
          from_address,
          %{}
        }],
        user_agent: user_agent,
        max_forwards: 70
      },
      cseq: 0
    }
    {:ok, state}
  end

  @impl true
  def handle_cast(:register, state) do
    cseq = state.cseq + 1

    headers = %{ state.headers_base |
      to: put_elem(state.headers_base.from, 2, %{})
      call_id: state.call_id,
      method: :register,
      cseq: cseq,
      content_length: 0
    } |> create_headers()

    message = Sippet.Message.build_request(:register, "sip:sip.ifon.pl") |> Map.put(:headers, headers)
    
    Sippet.send(state.client_id, message)

    {:noreply, %{state | cseq: cseq}}
  end

  @impl true
  def handle_cast({:invite, phone_number}, state) do
    calee = Sippet.URI%{scheme: "SIP", userinfo: phone_number, host: state.registrar_credentials.domain},
    {body, content_length} = SDP.proposal(state.external_ip, state.rtp_port)
    cseq = state.cseq + 1

    headers = %{ state.headers_base |
      to: {"", calee, %{}},
      call_id: state.call_id,
      method: :register,
      cseq: cseq,
      content_length: content_length
    } |> create_headers()

    message = Sippet.Message.build_request(:invite, calee |> to_string())
    |> Map.put(:headers, headers)
    |> Sippet.Message.put_header(:content_type, "application/sdp")
    |> Map.replace(:body, body)
    
    Sippet.send(state.client_id, message)

    {:noreply, %{state | cseq: cseq, calee: calee}}
  end

  @impl true
  def handle_cast(:bye, state) do
    to_address = Sippet.URI%{scheme: "SIP", userinfo: phone_number, host: state.registrar_credentials.domain},
    cseq = state.cseq + 1

    headers = %{ state.headers_base |
      to: {"", to_address, %{}},
      call_id: state.call_id,
      method: :register,
      cseq: cseq,
      content_length: 0
    } |> create_headers()

    message = Sippet.Message.build_request(:bye, to_address |> to_string())
    |> Map.put(:headers, headers)
    
    Sippet.send(state.client_id, message)

    {:noreply, %{state | cseq: cseq}}
  end

  @impl true
  def handle_cast({:respond, code, request, body}, state) do
    message = Sippet.Message.to_response(request, code)
    Sippet.send(state.client_id, message)
    {:noreply, state}
  end

  @impl true
  def handle_cast({:handle_response, %Sippet.Message{headers: %{cseq: cseq_resp}} = resp}, %{cseq: cseq_send}) when cseq_resp != cseq_send do
    Logger.warn("Received response doesn't match send cseq value\n#{inspect(resp)}")
    {:noreply, state}
  end

  def handle_cast({:handle_response, response}, state) do
    %Sippet.Message{
      start_line: %Sippet.Message.StatusLine{status_code: status_code, reason_phrase: reason_phrase}
    } = response

    if status_code == 401 && Sippet.Message.has_header?(response, :www_authenticate) do
      apply_digest_auth(response, state)
    else
      send(Client.registry_id(state.client_id), {:response, status_code, reason_phrase})
    end

    if state.cseq.method == :invite do
      send_ack(
        state.calee,
        %{state.headers_base | to: Sippet.Message.get_header(response, :to)},
        state.call_id,
        state.cseq
      )
      if status_code == 200 do
        case SDP.parse(response.body) do
          {:ok, connection_info} ->
            send(Client.registry_id(state.client_id), {:call_ready, connection_info})
          {:error, reason} ->
            send(Client.registry_id(state.client_id), {:sdp_mismatch, reason})
        end
      end
    end
    {:noreply, state}
  end

  @impl true
  def handle_cast({:send, code, req}, %{call_id: call_id, client_id: client_id} = state) do
    message = Sippet.Message.to_response(req, code)
    Sippet.send(client_id, message)
    {:noreply, state}
  end
  

end
