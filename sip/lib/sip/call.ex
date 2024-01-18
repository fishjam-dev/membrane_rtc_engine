defmodule Membrane.RTC.Engine.Endpoint.SIP.Call do
  @moduledoc false

  require Membrane.Logger

  alias Membrane.Logger
  alias Membrane.RTC.Engine.Endpoint.SIP
  alias Membrane.RTC.Engine.Endpoint.SIP.Call.{Auth, Headers, Settings}
  alias Membrane.RTC.Engine.Endpoint.SIP.{CallRegistry, SippetCore}

  @type id :: String.t()

  defmodule State do
    @moduledoc false
    use Bunch.Access

    @type t :: %__MODULE__{
            endpoint: pid(),
            rtp_port: 1..65_535,
            sip_port: 1..65_535,
            registrar_credentials: SIP.RegistrarCredentials.t(),
            external_ip: String.t(),
            register_interval_ms: non_neg_integer(),
            phone_number: String.t() | nil,
            call_id: SIP.Call.id(),
            callee: Sippet.URI.t() | nil,
            to: Sippet.Message.name_uri_params() | nil,
            target: {:udp, String.t(), non_neg_integer()} | nil,
            route: [Sippet.Message.name_uri_params()] | nil,
            headers_base: Headers.t(),
            cseq: non_neg_integer(),
            last_message: Sippet.Message.t() | nil,
            # Pending requests:
            #   %{{cseq, method} => time when request was made
            #             or when last provisional response to the request was received}
            pending_requests: %{{non_neg_integer(), atom()} => integer()}
          }

    @enforce_keys [
      :endpoint,
      :rtp_port,
      :sip_port,
      :registrar_credentials,
      :external_ip,
      :register_interval_ms,
      :phone_number,
      :call_id,
      :callee,
      :to,
      :target,
      :route,
      :headers_base,
      :cseq,
      :last_message,
      :pending_requests
    ]
    defstruct @enforce_keys
  end

  @type state :: State.t()

  @callback start_link(Settings.t()) :: {id(), pid()}
  @callback after_init(state :: state()) :: state()
  @callback handle_request(
              method :: atom(),
              request :: Sippet.Message.request(),
              state :: state()
            ) :: state()
  @callback handle_response(
              method :: atom(),
              status_code :: pos_integer(),
              response :: Sippet.Message.response(),
              state :: state()
            ) :: state()

  defmacro __using__(_opts) do
    quote location: :keep do
      use GenServer

      require Membrane.Logger

      alias Membrane.Logger
      alias Membrane.RTC.Engine.Endpoint.SIP

      @behaviour SIP.Call

      @impl SIP.Call
      def start_link(settings) do
        call_id = Sippet.Message.create_call_id()
        Logger.debug("SIP Client: Starting call process with id #{inspect(call_id)}")

        {:ok, pid} =
          GenServer.start_link(__MODULE__, {call_id, settings},
            name: SIP.Call.registry_id(call_id)
          )

        {call_id, pid}
      end

      @impl SIP.Call
      def after_init(state) do
        state
      end

      @impl SIP.Call
      def handle_request(_method, request, state) do
        Logger.warning("SIP Client: handle_request not implemented for #{inspect(request)}")
        state
      end

      @impl SIP.Call
      def handle_response(_method, status_code, response, state) do
        SIP.Call.handle_generic_response(status_code, response, state)
      end

      defoverridable after_init: 1, handle_request: 3, handle_response: 4

      @impl GenServer
      def init({call_id, settings}) do
        state = SIP.Call.init_state(call_id, settings)
        {:ok, __MODULE__.after_init(state)}
      end

      defguardp is_request_pending(state, cseq) when is_map_key(state.pending_requests, cseq)

      @impl GenServer
      def handle_cast({:response, %{headers: %{cseq: cseq}}}, state)
          when not is_request_pending(state, cseq) do
        Logger.warning(
          "SIP Client: Received response with CSeq #{inspect(cseq)}, for which there is no pending request. Ignoring."
        )

        {:noreply, state}
      end

      @impl GenServer
      def handle_cast({:response, %{headers: %{cseq: {_cseq, method}}} = response}, state) do
        Logger.debug("Received response in call: #{inspect(response)}")

        state = SIP.Call.process_response(response, state)

        status_code = response.start_line.status_code
        state = __MODULE__.handle_response(method, status_code, response, state)

        {:noreply, state}
      end

      @impl GenServer
      def handle_cast({:request, %{headers: %{cseq: {_cseq, method}}} = request}, state) do
        Logger.debug("Received request in call: #{inspect(request)}")
        {:noreply, __MODULE__.handle_request(method, request, state)}
      end

      @impl GenServer
      def handle_info({:timeout, cseq}, state) when is_request_pending(state, cseq) do
        if SIP.Call.timeout?(cseq, state) do
          raise "SIP Client: Timeout. Received no response for request with CSeq #{inspect(cseq)}"
        end

        {:noreply, state}
      end

      @impl GenServer
      def handle_info({:timeout, _cseq}, state) do
        {:noreply, state}
      end
    end
  end

  @timeout_ms 32_000

  ## MANAGEMENT API

  @spec stop(id()) :: :ok
  def stop(call_id) do
    GenServer.stop(registry_id(call_id))
  end

  @spec exists?(id()) :: boolean()
  def exists?(call_id) do
    Registry.lookup(CallRegistry, call_id) != []
  end

  ## INCOMING API

  @spec handle_request(id(), term()) :: :ok
  def handle_request(call_id, request) do
    GenServer.cast(registry_id(call_id), {:request, request})
  end

  @spec handle_response(id(), term()) :: :ok
  def handle_response(call_id, response) do
    GenServer.cast(registry_id(call_id), {:response, response})
  end

  ## PRIVATE API (for modules implementing the behaviour)

  @spec registry_id(id()) :: {:via, module(), term()}
  def registry_id(call_id) do
    {:via, Registry, {CallRegistry, call_id}}
  end

  @spec init_state(id(), Settings.t()) :: state()
  def init_state(call_id, settings) do
    from_address = %Sippet.URI{
      scheme: "sip",
      userinfo: settings.registrar_credentials.username,
      host: settings.external_ip,
      port: settings.sip_port
    }

    callee =
      if is_nil(settings.phone_number),
        do: nil,
        else: %{settings.registrar_credentials.uri | userinfo: settings.phone_number}

    settings
    |> Map.from_struct()
    |> Map.merge(%{
      call_id: call_id,
      callee: callee,
      to: nil,
      target: nil,
      route: nil,
      headers_base: Headers.create_headers_base(from_address),
      cseq: 0,
      last_message: nil,
      pending_requests: %{}
    })
    |> then(&struct!(State, &1))
  end

  @spec build_headers(atom(), state(), map(), String.t()) :: map()
  def build_headers(
        method,
        state,
        override_headers \\ %{},
        branch \\ Sippet.Message.create_branch()
      ) do
    # XXX: Consider optimising the digest auth process
    # (right now, we do the exchange `request, 401, request with digest` every time)

    headers =
      state.headers_base
      |> Map.merge(%{
        to: state.to || {"", state.callee, %{}},
        call_id: state.call_id,
        cseq: {state.cseq + 1, method},
        content_length: 0
      })
      |> Map.merge(override_headers)
      |> update_in([:via], fn via -> [Tuple.append(via, %{"branch" => branch})] end)

    if is_nil(state.route) do
      headers
    else
      Map.put(headers, :route, state.route)
    end
  end

  @spec make_request(Sippet.Message.request(), state(), boolean()) :: state() | no_return()
  def make_request(message, state, use_target? \\ true) do
    message =
      if use_target? and state.target != nil,
        do: Map.put(message, :target, state.target),
        else: message

    with :ok <- SippetCore.send_message(message) do
      cseq = message.headers.cseq

      Process.send_after(self(), {:timeout, cseq}, @timeout_ms)

      pending_requests =
        Map.put(state.pending_requests, cseq, System.monotonic_time(:millisecond))

      %{state | cseq: elem(cseq, 0), last_message: message, pending_requests: pending_requests}
    else
      error ->
        Logger.debug("Send failed with message: #{inspect(message)}")
        raise "SIP Client: Unable to send message: #{inspect(error)}"
    end
  end

  # Generic processing of _all_ responses
  @spec process_response(Sippet.Message.response(), state()) :: state()
  def process_response(response, state) do
    state
    |> update_routing(response)
    |> update_pending_requests(response)
  end

  @spec timeout?(non_neg_integer(), state()) :: boolean() | no_return()
  def timeout?(cseq, state) do
    System.monotonic_time(:millisecond) >= Map.fetch!(state.pending_requests, cseq) + @timeout_ms
  end

  @spec handle_generic_response(pos_integer(), Sippet.Message.response(), state()) ::
          state() | no_return()
  def handle_generic_response(status_code, response, state) do
    case status_code do
      success when success in [200, 204] ->
        state

      unauthorized when unauthorized in [401, 407] ->
        handle_unauthorized(response, state)

      redirect when redirect in 300..399 ->
        raise """
        SIP Client: Received redirection response with code #{status_code}.
        Redirections need to be handled by the module implementing the `Call` behaviour
        """

      _other_failure ->
        raise """
        SIP Client: Received unhandled failure response
          with code #{status_code} (#{inspect(response.start_line.reason_phrase)}):
          #{inspect(response)}
        """
    end
  end

  defp handle_unauthorized(response, state) do
    headers = state.last_message.headers
    authorization_headers = headers[:authorization] || headers[:proxy_authorization] || []

    cond do
      not Enum.empty?(authorization_headers) ->
        raise "SIP Client: Unable to authorize using digest auth (incorrect credentials?)"

      Sippet.Message.has_header?(response, :www_authenticate) or
          Sippet.Message.has_header?(response, :proxy_authenticate) ->
        request = Auth.apply_digest(state.last_message, response, state.registrar_credentials)
        make_request(request, state)

      true ->
        raise "SIP Client: Unable to authorize using digest auth (no `www-authenticate` or `proxy-authenticate` header present)"
    end
  end

  defp update_pending_requests(state, response) do
    cseq = response.headers.cseq

    pending_requests =
      if response.start_line.status_code in 100..199 do
        Process.send_after(self(), {:timeout, cseq}, @timeout_ms)
        Map.put(state.pending_requests, cseq, System.monotonic_time(:millisecond))
      else
        Map.delete(state.pending_requests, cseq)
      end

    %{state | pending_requests: pending_requests}
  end

  # According to RFC 3261 section 12.2.1.1
  # https://datatracker.ietf.org/doc/html/rfc3261#section-12.2.1.1
  defp update_routing(state, response) when is_map_key(response.headers, :record_route) do
    %Sippet.Message{
      headers: %{
        to: to,
        record_route: record_route,
        contact: [{_name, contact_uri, _params} | _] = contact
      }
    } = response

    route = Enum.reverse(record_route)
    [{_name, first_hop_uri, _params} | _] = route

    loose_routing? = loose_routing?(first_hop_uri)

    {state, route} =
      if loose_routing? do
        Logger.debug("SIP Client: using loose routing")

        {%{state | callee: contact_uri, target: {:udp, first_hop_uri.host, first_hop_uri.port}},
         route}
      else
        Logger.debug("SIP Client: using strict routing")
        callee = first_hop_uri |> Map.put(:parameters, nil)

        {%{state | callee: callee}, Enum.drop(route, 1) ++ contact}
      end

    %{state | route: route, to: to}
  end

  defp update_routing(state, response) do
    state = %{state | to: response.headers.to}

    if is_map_key(response.headers, :contact) do
      [{_name, contact_uri, _params} | _] = response.headers.contact

      %{state | callee: contact_uri}
    else
      state
    end
  end

  defp loose_routing?(first_hop_uri) do
    params = Map.fetch!(first_hop_uri, :parameters) || ""

    params
    |> String.split(";")
    |> Enum.member?("lr")
  end
end
