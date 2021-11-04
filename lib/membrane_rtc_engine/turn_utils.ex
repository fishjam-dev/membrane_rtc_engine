defmodule Membrane.RTC.Engine.TurnUtils do
  @moduledoc false

  @spec create_credentials(binary(), binary()) :: {binary(), binary()}
  def create_credentials(name, secret) do
    duration =
      DateTime.utc_now()
      |> DateTime.to_unix()
      |> tap(fn unix_timestamp -> unix_timestamp + 24 * 3600 end)

    username = "#{duration}:#{name}"

    password =
      :crypto.mac(:hmac, :sha, secret, username)
      |> Base.encode64()

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
    do: :stun_listener.del_listener(turn.server_addr, turn.server_port, turn.relay_type)
end
