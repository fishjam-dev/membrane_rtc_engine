defmodule Membrane.RTC.Engine.Turn do
  def create_credentials(name, secret) do
    duration =
      DateTime.utc_now()
      |> DateTime.to_unix()
      |> tap(fn unix_timestamp -> unix_timestamp + 24 * 3600 end)

    username = "#{duration}:#{name}"
    IO.inspect(secret, label: "dupa turn.ex 9 secret")
    IO.inspect(username, label: "dupa turn.ex 10 username")

    password =
      :crypto.mac(:hmac, :sha, secret, username)
      |> IO.inspect(label: "dupa password is")
      |> Base.encode64()
      |> IO.inspect(label: " dupa password after encoding is")

    # password = :crypto.mac(:hmac, :sha, secret, username) |> IO.inspect(label: "dupa password is")
    {username, password}
  end
end
