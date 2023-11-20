defmodule Membrane.RTC.Engine.Endpoint.SIP.Client do
  @moduledoc false
  # use Bunch.Access
  use GenServer
  use Sippet.Core

  require Membrane.Logger

  alias Membrane.RTC.Engine.Endpoint.SIP.{Call, RegistrarCredentials}

  defmodule Settings do

    @type t :: %__MODULE__{
            endpoint_pid: pid(),
            rtp_port: port(),
            sip_port: port(),
            registrar_credentials: RegistrarCredentials.t()
          }

    @enforce_keys [
      :endpoint_pid,
      :rtp_port,
      :sip_port,
      :registrar_credentials
    ]

    defstruct @enforce_keys
  
  end

  @register_interval 45


  def start_link(id, settings) do
    id = String.to_atom(id)
    GenServer.start_link(__MODULE__, [id, settings], name: {:via, Registry, {SipEndpoint.ClientRegistry, id}})
  end

  @impl true
  def init(id, settings)
    state = %{ settings |
      id: id,
      register_call_id: Sippet.Message.create_call_id(),
      call_id: nil,
      registered: false
    }

    Sippet.register_core(id, __MODULE__)
    keep_me_registered(id, state.registrar_credentials, state.sip_port, state.register_call_id)
    {noreply, state}
  end

  @impl true
  def handle_info({:ok, call_id}, %{register_call_id: register_call_id} = state) when register_call_id == call_id do
    send(state.endpoint, :client_ready)
    {:noreply, %{state | registerd: true}}
  end
  
  @impl true
  def handle_info({:ok, call_id}, state) do
    send(state.endpoint, {:call_ready, call_id})
    {:noreply, state}
  end

  @impl true
  def handle_info({:ok, call_id}, %{register_call_id: register_call_id} = state) when register_call_id == call_id do
    send(state.endpoint, :client_unregistered)
    {:noreply, %{state | registerd: false}}
  end

  @impl true
  def handle_info({:end, call_id}, state) do
    send(state.endpoint, {:call_end, call_id})
    {:noreply, %{state | call_id: nil}}
  end

  @impl true
  def handle_info({:message_client, {:dial, phone_number}}, state) do
    {:ok, call_id} = Call.start_link(client_id, registrar_credentials, sip_port)
    Call.invite(call_id, phone_number)
    {:noreply, %{state | call_id: call_id}}
  end

  @impl true
  def handle_info({:message_client, :hang_up}, state) do
    Call.bye(state.call_id, phone_number)
    {:noreply, state}
  end

  @impl true
  def handle_info(_msg, state), do: {noreply, state}

  @impl true
  def handle_call(:is_registered, _from, state) do
    {:reply, state.registered, state}
  end

  def registered?(id)
    id = String.to_existing_atom(id)
    GenServer.call({:via, Registry, {SipEndpoint.ClientRegistry, id}}, :is_registered)
  end

  def registry_name(id)
    {:via, Registry, {SipEndpoint.ClientRegistry, id}}
  end

  def receive_request(%Sippet.Message{start_line: %Sippet.Message.RequestLine{method: method}} = incoming_request, server_key) do
    Logger.info("Sipped receive_request: #{inspect(incoming_request)}")
    Logger.info("from: #{inspect(server_key)}")
    call_id = Sippet.Message.get_header(incoming_request, :call_id)
    cond do
      # We don't support incoming calls until new sip client implementation
      Call.exists?(call_id) -> handle_request(method, call_id, incoming_request)
      true -> :unsupported
    end
  end

  def receive_response(incoming_response, client_key) do
    Logger.info("Sipped receive_response: #{inspect(incoming_response)}")
    Logger.info("from: #{inspect(server_key)}")
    call_id = Sippet.Message.get_header(incoming_request, :call_id)
    if  do

      true ->  SipEndpoint.Call.apply_digest_auth(incoming_response)
      false ->  SipEndpoint.Call.handle_response(incoming_response)
    end
  end

  def receive_error(reason, client_or_server_key) do
    Logger.info("Sipped receive_error: #{inspect(reason)}")
    Logger.info("from: #{inspect(client_or_server_key)}")
    # route the error to your UA or proxy process
  end

  defp handle_request(:notify, call_id, incoming_request), do: Call.handle_notify(call_id, incoming_request)
  defp handle_request(:ack, _call_id, _incoming_request), do: :ok
  defp handle_request(:bye, call_id, incoming_request), do: Call.handle_bye(call_id, incoming_request)
  
  defp handle_request(:cancel, call_id, incoming_request), do: Call.handle_bye(call_id, incoming_request)

  # We don't support incoming calls until new sip client implementation
  defp handle_request(:invite, _call_id, _incoming_request), do: :unsupported
  # defp handle_request(:invite, incoming_request) do
  #   case SDP.extract_data(req.body)
  #     {:ok, %SDP.Data{} = data} ->
  #       Call.handle_invite(incoming_request)
  #     {:error, reason} ->
  #       Logger.error(reason)
  #       Call.respond(488, incoming_request)
  #   end
  # end
  defp handle_request(_method, call_id, incoming_request), do: Call.handle_bye(call_id, incoming_request)

  defp keep_me_registered(id, registrar_credentials, sip_port, register_call_id) do
    Membrane.Logger.debug("ConnectionManager: Starting Keep alive process")
    {keep_alive, _ref} = spawn_monitor(fn -> register_client(id, registrar_credentials, sip_port, register_call_id) end)
  end

  defp register_client(id, registrar_credentials, sip_port, register_call_id) do
    case Call.register(id, registrar_credentials, sip_port, register_call_id) do
      Process.sleep(@register_interval)
      register_client(id, registrar_credentials, sip_port, register_call_id)
    end
  end

end