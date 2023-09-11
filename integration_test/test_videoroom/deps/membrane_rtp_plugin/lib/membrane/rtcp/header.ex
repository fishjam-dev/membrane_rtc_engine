defmodule Membrane.RTCP.Header do
  @moduledoc """
  Struct describing 32-bit header common to all RTCP packets
  """

  @enforce_keys [:packet_type, :packet_specific]
  defstruct @enforce_keys

  @type packet_type_t :: 200..206

  @type packet_specific_t :: non_neg_integer()

  @type t :: %__MODULE__{
          packet_specific: packet_specific_t(),
          packet_type: packet_type_t()
        }

  @spec parse(binary()) ::
          {:ok, %{header: t(), padding?: boolean, body_size: pos_integer}}
          | :error
  def parse(<<2::2, padding::1, packet_specific::5, pt::8, length::16>>) do
    {:ok,
     %{
       header: %__MODULE__{
         packet_specific: packet_specific,
         packet_type: pt
       },
       padding?: padding == 1,
       body_size: length * 4
     }}
  end

  def parse(_binary) do
    :error
  end

  @spec serialize(t(), body_size: pos_integer(), padding?: boolean()) :: binary()
  def serialize(%__MODULE__{} = header, opts) do
    padding = if Keyword.get(opts, :padding?), do: 1, else: 0
    size = Keyword.fetch!(opts, :body_size)

    unless rem(size, 4) == 0 do
      raise "RTCP packet body size must be divisible by 4, got: #{size}"
    end

    <<2::2, padding::1, header.packet_specific::5, header.packet_type::8, div(size, 4)::16>>
  end
end
