defmodule Membrane.ICE.Utils do
  @moduledoc false

  require Membrane.Logger

  alias Membrane.ICE

  @numbers_and_letters Enum.concat([?0..?9, ?a..?z, ?A..?Z])

  @spec generate_credentials(binary(), binary()) :: {binary(), binary()}
  def generate_credentials(name, secret) do
    duration =
      DateTime.utc_now()
      |> DateTime.to_unix()
      |> tap(fn unix_timestamp -> unix_timestamp + 24 * 3600 end)

    username = "#{duration}:#{name}"
    password = :stun_codec.generate_user_password(secret, username)
    {username, password}
  end

  @spec generate_secret() :: binary()
  def generate_secret() do
    symbols = '0123456789abcdef'

    1..20
    |> Enum.map(fn _i -> Enum.random(symbols) end)
    |> to_string()
  end

  @spec start_integrated_turn(binary(), list()) :: {:ok, :inet.port_number(), pid()}
  def start_integrated_turn(secret, opts \\ []),
    do: :turn_starter.start(secret, opts)

  @spec stop_integrated_turn(map()) :: :ok
  def stop_integrated_turn(turn),
    do: :turn_starter.stop(turn.server_addr, turn.server_port, turn.relay_type)

  @spec generate_fake_ice_candidate({:inet.ip4_address(), :inet.port_number()}) :: binary()
  def generate_fake_ice_candidate({ip, port}) do
    # `2015363327` in string below is candidate priority - because of the fact, that we are sending
    # only one candidate, its value is of no great importance
    ip
    |> Tuple.to_list()
    |> Enum.join(".")
    |> then(&"a=candidate:1 1 UDP 2015363327 #{&1} #{port} typ host")
  end

  @spec send_binding_success(pid(), binary(), number(), number(), binary()) :: any()
  def send_binding_success(alloc_pid, pwd, magic, trid, username) when is_pid(alloc_pid) do
    [class: :response, username: username]
    |> send_connectivity_check(alloc_pid, pwd, magic, trid)
  end

  @spec send_binding_request(pid(), binary(), number(), number(), binary(), number()) :: any()
  def send_binding_request(alloc_pid, pwd, magic, trid, username, priority)
      when is_pid(alloc_pid) do
    [
      class: :request,
      username: username,
      priority: priority,
      ice_controlled: true
    ]
    |> send_connectivity_check(alloc_pid, pwd, magic, trid)
  end

  @spec send_binding_indication(pid(), binary(), number(), number()) :: any()
  def send_binding_indication(alloc_pid, pwd, magic, trid) when is_pid(alloc_pid) do
    [class: :indication]
    |> send_connectivity_check(alloc_pid, pwd, magic, trid)
  end

  @spec send_error_role_conflict(pid(), binary(), number(), number()) :: any()
  def send_error_role_conflict(alloc_pid, pwd, magic, trid) when is_pid(alloc_pid) do
    [
      class: :error,
      error_code: 487
    ]
    |> send_connectivity_check(alloc_pid, pwd, magic, trid)
  end

  @spec generate_transaction_id() :: number()
  def generate_transaction_id() do
    # RFC 5389, 3: transaction ID [...] is a randomly selected 96-bit number
    <<tr_id::12*8>> = :crypto.strong_rand_bytes(12)
    tr_id
  end

  # [RFC8445] requires the "ice-ufrag" attribute to contain at least 24
  # bits of randomness, and the "ice-pwd" attribute to contain at least
  # 128 bits of randomness.
  @spec generate_ice_ufrag() :: binary()
  def generate_ice_ufrag(), do: random_readable_binary(4)

  @spec generate_ice_pwd() :: binary()
  def generate_ice_pwd(), do: random_readable_binary(22)

  @spec is_dtls_hsk_packet(binary()) :: boolean()
  def is_dtls_hsk_packet(<<head, _rest::binary>> = packet),
    do: head in 20..63 and byte_size(packet) >= 13

  @spec start_integrated_turn_servers(
          [:udp | :tcp | :tls],
          ICE.Endpoint.integrated_turn_options_t(),
          Keyword.t()
        ) :: [any()]
  def start_integrated_turn_servers(turn_transports, turn_options, additional_options \\ []) do
    ip = turn_options[:ip] || {0, 0, 0, 0}
    mock_ip = turn_options[:mock_ip] || ip
    {min_port, max_port} = turn_options[:ports_range] || {50_000, 59_999}
    medium = trunc((min_port + max_port) / 2)

    client_port_range = {min_port, medium}

    alloc_port_range =
      if medium == max_port,
        do: {medium, max_port},
        else: {medium + 1, max_port}

    turns =
      turn_transports
      |> Enum.map(fn transport ->
        secret = generate_secret()

        opts =
          [
            parent: additional_options[:parent],
            client_port: additional_options[:port],
            client_port_range: client_port_range,
            alloc_port_range: alloc_port_range,
            ip: ip,
            mock_ip: mock_ip,
            transport: transport,
            certfile: turn_options[:cert_file],
            parent_resolver: &ICE.CandidatePortAssigner.get_candidate_port_owner/1
          ]
          |> Enum.filter(fn
            {_key, nil} -> false
            _other -> true
          end)

        {:ok, port, pid} = start_integrated_turn(secret, opts)

        %{
          relay_type: transport,
          secret: secret,
          server_addr: ip,
          mocked_server_addr: mock_ip,
          server_port: port,
          pid: pid
        }
      end)

    Enum.each(turns, fn turn ->
      addr = Tuple.to_list(turn.server_addr) |> Enum.join(".")

      Membrane.Logger.debug(
        "Starting #{turn.relay_type} TURN Server at #{inspect(addr)}:#{turn.server_port}"
      )
    end)

    turns
  end

  defp send_connectivity_check(attrs, alloc_pid, pwd, magic, trid) do
    attrs =
      [
        ice_pwd: pwd,
        magic: magic,
        trid: trid
      ] ++ attrs

    send(alloc_pid, {:send_connectivity_check, attrs})
  end

  defp random_readable_binary(len),
    do: to_string(for _i <- 1..len, do: Enum.random(@numbers_and_letters))
end
