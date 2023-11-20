defmodule Membrane.RTC.Engine.Endpoint.SIP.Call do
  @moduledoc false
  use Bunch.Access

  require Membrane.Logger

  alias Membrane.RTC.Engine.Endpoint.SIP.SDP

  @type t :: %__MODULE__{
          id: binary(),
          callee: binary(),
          endpoint: pid(),
          rtp_in_port: port()
        }

  @enforce_keys [
    :id,
    :callee,
    :endpoint
  ]

  defstruct @enforce_keys

  def register(call_id) do
    GenServer.cast(pid, :register)
    {:ok, call_id}
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

  # def apply_digest_auth(resp) do
  #   GenServer.cast({:via, Registry, {SipEndpoint.CallRegistry, call_id}}, {:apply_digest, resp})
  # end

  def handle_response(call_id, response) do
    GenServer.cast({:via, Registry, {SipEndpoint.CallRegistry, call_id}}, {:handle_response, response})
  end

  # def handle_invite(call_id, request) do  
    # GenServer.cast({:via, Registry, {SipEndpoint.CallRegistry, call_id}}, {:send, 100})
  # end

  def handle_notify(call_id, req) do
    GenServer.cast({:via, Registry, {SipEndpoint.CallRegistry, call_id}}, {:send, 200, req})
  end

  def handle_bye(call_id, req) do
    send(Client.registry_name(state.client_id), {:end, call_id})
    respond(call_id, 200, req)
    GenServer.stop({:via, Registry, {SipEndpoint.CallRegistry, call_id}}, :shutdown)
  end

  def respond(call_id, code, req) do
    GenServer.cast({:via, Registry, {SipEndpoint.CallRegistry, call_id}}, {:send, code, req})
  end

  def start_link(call_id, client_id, registrar_credentials, sip_port, rtp_port) do
    GenServer.start_link(__MODULE__, [call_id, client_id, registrar_credentials, sip_port, rtp_port], name: {:via, Registry, {SipEndpoint.CallRegistry, call_id}})
  end

  defp send_ack(response, headers_base) do
    
    headers_base = %{ headers_base |
      call_id: state.call_id,
      method: :register,
      cseq: cseq,
      content_length: 0
    }

    headers = create_headers(state.headers_base, call_id, method, cseq)
    message = Sippet.Message.build_request(:ack, calee)
      |> Map.put(:headers, base_headers)
      |> Sippet.Message.put_header(:cseq, {cseq, :ack})
      |> Sippet.Message.put_header(:content_length, 0)
    Sippet.send(client_id, message)
  end

  defp apply_digest_auth(response, state) do
    new_cseq = state.cseq
    {request, new_cseq} = new_request(state)
    {:ok, request} = Sippet.DigestAuth.make_request(request, response, &get_userinfo/1)
    # message = Sippet.Message.update_header(new_req, :cseq, fn ({_cseq, method}) -> {cseq, method} end)
      # |> Sippet.Message.put_header(:via, [
      #   {{2, 0}, :udp, {"49.13.56.174", 5060}, %{"branch" => Sippet.Message.create_branch()}}
      # ])
      # |> IO.inspect()
    Sippet.send(state.client_id, request)
    new_cseq
  end

  defp new_request(method, from, to, cseq) do

    from = {"", Sippet.URI.parse!("sip:78054@sip.ifon.pl"), %{"tag" => Sippet.Message.create_tag()}}

    message = Sippet.Message.build_request(:register, "sip:sip.ifon.pl")
    |> Sippet.Message.put_header(:max_forwards, 70)
    |> Sippet.Message.put_header(:from, {"", Sippet.URI.parse!("sip:78054@sip.ifon.pl"), %{"tag" => Sippet.Message.create_tag()}})
    |> Sippet.Message.put_header(:to, {"", Sippet.URI.parse!("sip:78054@sip.ifon.pl"), %{}})
    |> Sippet.Message.put_header(:via, [
      {{2, 0}, :udp, {"49.13.56.174", 5060}, %{"branch" => Sippet.Message.create_branch()}}
    ])
    |> Sippet.Message.put_header(:contact, [{"", Sippet.URI.parse!("sip:78054@49.13.56.174"), %{}}])
    |> Sippet.Message.put_header(:user_agent, user_agent())
    |> Sippet.Message.put_header(:call_id, call_id)
    |> Sippet.Message.put_header(:cseq, {cseq, :register})
    |> Sippet.Message.put_header(:content_length, 0)



    to = {"", Sippet.URI.parse!(addr), %{}}
    message = Sippet.Message.build_request(method, addr)
      |> Sippet.Message.put_header(:max_forwards, 70)
      |> Sippet.Message.put_header(:from, from)
      |> Sippet.Message.put_header(:to, to)
      |> Sippet.Message.put_header(:via, [
        {{2, 0}, :udp, {"49.13.56.174", 5060}, %{"branch" => Sippet.Message.create_branch()}}
      ])
      |> Sippet.Message.put_header(:contact, [{"", Sippet.URI.parse!("sip:78054@49.13.56.174"), %{}}] )
      |> Sippet.Message.put_header(:user_agent, user_agent())
      |> Sippet.Message.put_header(:call_id, call_id)
      |> Sippet.Message.put_header(:cseq, cseq)
      |> Sippet.Message.put_header(:content_length, content_length)
      |> Sippet.Message.put_header(:content_type, "application/sdp")
      |> Map.replace(:body, body)








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

  defp new_cseq(method), do: {1, method}
  defp bump_cseq({i, method}), do: {i + 1, method}


  # def send_bye()

  @impl true
  def init(call_id, client_id, %RegistrarCredentials{domain: domain, username: username}registrar_credentials, sip_port, rtp_port) do
    
    from_address = Sippet.URI%{scheme: "SIP", userinfo: username, host: external_ip, port: sip_port},
    caller_identifier = "Jellyfish"
    tag = Sippet.Message.create_tag()
    user_agent = "Jellyfish/SIP/0.3"

    state = %{
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

    headers_base = %{ state.headers_base |
      to: put_elem(state.headers_base.from, 2, %{})
      call_id: state.call_id,
      method: :register,
      cseq: cseq,
      content_length: 0
    }

    headers = create_headers(headers_base)
    
    message = Sippet.Message.build_request(:register, "sip:sip.ifon.pl") |> Map.put(:headers, headers)
    
    Sippet.send(state.client_id, message)

    {:noreply, %{state | cseq: cseq}}

  #   message = Sippet.Message.build_request(:register, "sip:sip.ifon.pl")
  #     |> Sippet.Message.put_header(:max_forwards, 70)
  #     |> Sippet.Message.put_header(:from, {"", Sippet.URI.parse!("sip:78054@sip.ifon.pl"), %{"tag" => Sippet.Message.create_tag()}})
  #     |> Sippet.Message.put_header(:to, {"", Sippet.URI.parse!("sip:78054@sip.ifon.pl"), %{}})
  #     |> Sippet.Message.put_header(:via, [
  #       {{2, 0}, :udp, {"49.13.56.174", 5060}, %{"branch" => Sippet.Message.create_branch()}}
  #     ])
  #     |> Sippet.Message.put_header(:contact, [{"", Sippet.URI.parse!("sip:78054@49.13.56.174"), %{}}])
  #     |> Sippet.Message.put_header(:user_agent, "")
  #     |> Sippet.Message.put_header(:call_id, call_id)
  #     |> Sippet.Message.put_header(:cseq, {cseq, :register})
  #     |> Sippet.Message.put_header(:content_length, 0)
  #     |> IO.inspect()
  #   Sippet.send(:sip_endpoint, message)
  #   {:noreply, %{state | cseq: cseq + 1, last_message: message}}
  end

  @impl true
  def handle_cast({:respond, code, request, body}, state) do
    message = Sippet.Message.to_response(request, code)
    Sippet.send(:sip_endpoint, message)
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
      send(Client.registry_name(state.client_id), {:response, status_code, reason_phrase})
    end

    if state.cseq.method == :invite do
      headers_base
      send_ack(
        response,
        %{state.headers_base | to: Sippet.Message.get_header(response, :to)},
        state.call_id,
        state.cseq
      )
    end
    {:noreply, state}
  end

  # @impl true
  # def handle_cast({:ack, resp}, %{base_headers: base_headers, client_id: client_id} = state) do


  # end

  @impl true
  def handle_cast({:trying, req}, %{call_id: call_id} = state) do
    message = Sippet.Message.to_response(req, 100) |> IO.inspect()
    Sippet.send(:sip_endpoint, message)
    # should check if SDP meets our capabilities and respond with ringing if its ok and after notifying user. Now simplified.
    # GenServer.cast(self(), {:ringing, req})
    {:noreply, state}
    # {:noreply, %{state | cseq: cseq}}
  end

  @impl true
  def handle_cast({:send, code, req}, %{call_id: call_id} = state) do
    message = Sippet.Message.to_response(req, code)
    Sippet.send(:sip_endpoint, message)
    {:noreply, state}
  end
  

end
