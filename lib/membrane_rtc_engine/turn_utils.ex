defmodule Membrane.RTC.Engine.TurnUtils do
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
end
