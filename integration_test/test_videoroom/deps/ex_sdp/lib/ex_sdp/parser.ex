defmodule ExSDP.Parser do
  @moduledoc """
  This module is responsible for parsing SDP multimedia session.
  """
  use Bunch.Access

  require Logger

  alias ExSDP

  alias ExSDP.{
    Attribute,
    Bandwidth,
    ConnectionData,
    Encryption,
    Media,
    Origin,
    RepeatTimes,
    Timezone,
    Timing
  }

  @line_ending ["\r\n", "\r", "\n"]

  @doc """
  Parses SDP Multimedia Session.
  """
  @spec parse(binary()) ::
          {:ok, ExSDP.t()} | {:error, atom() | {:not_supported_addr_type, binary()}}
  def parse(binary) do
    binary
    |> String.split(@line_ending)
    |> do_parse()
  end

  defp do_parse(lines, session \\ struct(ExSDP))
  defp do_parse([""], session), do: {:ok, flip_media(session)}

  defp do_parse(lines, session) do
    case parse_line(lines, session) do
      {rest, %ExSDP{} = session} ->
        do_parse(rest, session)

      {:error, reason} ->
        {:error, {reason, List.first(lines)}}
    end
  end

  @doc """
  Parses SDP Multimedia Session raising an exception in case of failure.
  """
  @spec parse!(binary()) :: ExSDP.t()
  def parse!(binary) do
    binary
    |> String.split(@line_ending)
    |> do_parse!()
  end

  defp do_parse!(lines, session \\ struct(ExSDP))
  defp do_parse!([""], session), do: flip_media(session)

  defp do_parse!(lines, session) do
    case parse_line(lines, session) do
      {:error, reason} ->
        error_message = format_error(lines, reason)
        raise error_message

      {rest, %ExSDP{} = session} ->
        do_parse!(rest, session)
    end
  end

  defp parse_line(lines, session)

  defp parse_line(["v=" <> version | rest], spec),
    do: {rest, %ExSDP{spec | version: String.to_integer(version)}}

  defp parse_line(["o=" <> origin | rest], spec) do
    with {:ok, %Origin{} = origin} <- Origin.parse(origin) do
      {rest, %ExSDP{spec | origin: origin}}
    end
  end

  defp parse_line(["s=" <> session_name | rest], spec),
    do: {rest, %ExSDP{spec | session_name: session_name}}

  defp parse_line(["i=" <> session_information | rest], spec),
    do: {rest, %ExSDP{spec | session_information: session_information}}

  defp parse_line(["u=" <> uri | rest], spec),
    do: {rest, %ExSDP{spec | uri: uri}}

  defp parse_line(["e=" <> email | rest], spec),
    do: {rest, %ExSDP{spec | email: email}}

  defp parse_line(["p=" <> phone_number | rest], spec),
    do: {rest, %ExSDP{spec | phone_number: phone_number}}

  defp parse_line(["c=" <> connection_data | rest], spec) do
    with {:ok, %ConnectionData{} = connection_data} <- ConnectionData.parse(connection_data) do
      {rest, %ExSDP{spec | connection_data: connection_data}}
    end
  end

  defp parse_line(["b=" <> bandwidth | rest], %ExSDP{bandwidth: acc_bandwidth} = spec) do
    with {:ok, bandwidth} <- Bandwidth.parse(bandwidth) do
      {rest, %ExSDP{spec | bandwidth: [bandwidth | acc_bandwidth]}}
    end
  end

  defp parse_line(["t=" <> timing | rest], spec) do
    with {:ok, timing} <- Timing.parse(timing) do
      {rest, %ExSDP{spec | timing: timing}}
    end
  end

  defp parse_line(["r=" <> repeat | rest], %ExSDP{time_repeats: time_repeats} = spec) do
    with {:ok, repeats} <- RepeatTimes.parse(repeat) do
      {rest, %ExSDP{spec | time_repeats: [repeats | time_repeats]}}
    end
  end

  defp parse_line(["z=" <> timezones | rest], spec) do
    with {:ok, timezones} <- Timezone.parse(timezones) do
      {rest, %ExSDP{spec | time_zones_adjustments: timezones}}
    end
  end

  defp parse_line(["k=" <> encryption | rest], spec) do
    with {:ok, encryption} <- Encryption.parse(encryption) do
      {rest, %ExSDP{spec | encryption: encryption}}
    end
  end

  defp parse_line(["a=" <> attribute | rest], %{attributes: attrs} = session) do
    with {:ok, attribute} <- Attribute.parse(attribute) do
      {rest, %ExSDP{session | attributes: [attribute | attrs]}}
    end
  end

  defp parse_line(["m=" <> medium | rest], %ExSDP{media: media} = session) do
    with {:ok, medium} <- Media.parse(medium),
         {:ok, {rest, medium}} <- Media.parse_optional(rest, medium) do
      medium = Media.apply_session(medium, session)
      {rest, %ExSDP{session | media: [medium | media]}}
    end
  end

  defp format_error(["m=" <> _rest = line | rest], reason) do
    attributes =
      rest
      |> Enum.take_while(fn
        "" -> false
        line -> not String.starts_with?(line, "m=")
      end)
      |> Enum.join("\n")

    """
    Error while parsing media:
    #{line}

    Attributes:
    #{attributes}

    with reason: #{reason}
    """
  end

  defp format_error([line | _], reason) do
    """
    An error has occurred while parsing following SDP line:
    #{line}
    with reason: #{reason}
    """
  end

  defp flip_media(%{media: media} = session),
    do: %{session | media: Enum.reverse(media)}
end
