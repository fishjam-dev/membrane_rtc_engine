defmodule Membrane.RTC.Engine.TurnUtils do
  @moduledoc false

  @spec turn_credentials(binary(), binary()) :: {binary(), binary()}
  def turn_credentials(name, secret) do
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
end
