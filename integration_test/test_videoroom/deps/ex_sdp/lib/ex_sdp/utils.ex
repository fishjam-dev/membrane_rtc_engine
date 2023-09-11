defmodule ExSDP.Utils do
  @moduledoc false

  @spec split(String.t(), String.t() | [String.t()] | :binary.cp() | Regex.t(), any) ::
          {:error, :too_few_fields} | {:ok, [String.t()]}
  def split(origin, delim, expected_len) do
    split = String.split(origin, delim, parts: expected_len)
    if length(split) == expected_len, do: {:ok, split}, else: {:error, :too_few_fields}
  end

  @spec parse_payload_type(binary) :: {:ok, 0..127} | {:errror, :invalid_pt}
  def parse_payload_type(pt_string) do
    case Integer.parse(pt_string) do
      {pt, ""} when pt >= 0 and pt <= 127 -> {:ok, pt}
      _otherwise -> {:error, :invalid_pt}
    end
  end

  @spec parse_numeric_string(binary) :: {:error, :string_nan} | {:ok, integer}
  def parse_numeric_string(string) do
    case Integer.parse(string) do
      {number, ""} -> {:ok, number}
      _string_nan -> {:error, :string_nan}
    end
  end

  @spec parse_numeric_hex_string(binary) :: {:error, :string_not_hex} | {:ok, integer}
  def parse_numeric_hex_string(string) do
    case Integer.parse(string, 16) do
      {number, ""} -> {:ok, number}
      _string_not_hex -> {:error, :string_not_hex}
    end
  end

  @spec parse_numeric_bool_string(binary) :: {:error, :string_not_0_nor_1} | {:ok, boolean}
  def parse_numeric_bool_string(string) do
    case Integer.parse(string) do
      {0, ""} -> {:ok, false}
      {1, ""} -> {:ok, true}
      _string_not_0_nor_1 -> {:error, :string_not_0_nor_1}
    end
  end

  @spec parse_sprop_parameter_sets(binary) ::
          {:error, :invalid_sprop_parameter_sets} | {:ok, %{sps: binary, pps: binary}}
  def parse_sprop_parameter_sets(string) do
    result =
      String.split(string, ",", parts: 2)
      |> Enum.zip([:sps, :pps])
      |> Map.new(fn {encoded, type} ->
        decoded =
          case Base.decode64(encoded) do
            {:ok, decoded} -> decoded
            :error -> nil
          end

        {type, decoded}
      end)

    if Map.get(result, :sps) && Map.get(result, :pps) do
      {:ok, result}
    else
      {:error, :invalid_sprop_parameter_sets}
    end
  end
end
