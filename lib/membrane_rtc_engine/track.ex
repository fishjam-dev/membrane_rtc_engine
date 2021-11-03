defmodule Membrane.RTC.Engine.Track do
  @moduledoc """
  Module representing a WebRTC track.
  """
  alias ExSDP.Attribute.FMTP

  @enforce_keys [:type, :stream_id, :id, :fmtp]
  defstruct @enforce_keys ++ [encoding: nil, disabled?: false]

  @type id :: String.t()
  @type encoding :: :OPUS | :H264 | :VP8

  @type t :: %__MODULE__{
          type: :audio | :video,
          stream_id: String.t(),
          id: id,
          encoding: encoding,
          fmtp: FMTP,
          disabled?: boolean()
        }

  @doc """
  Creates a new track.

  Tracks belonging to the same stream should have the same `stream_id`,
  that can be generated with `stream_id/0`.
  """
  @spec new(
          :audio | :video,
          stream_id :: String.t(),
          id: String.t(),
          encoding: encoding,
          fmtp: FMTP
        ) :: t
  def new(type, stream_id, opts \\ []) do
    id = Keyword.get(opts, :id, Base.encode16(:crypto.strong_rand_bytes(8)))

    %__MODULE__{
      type: type,
      stream_id: stream_id,
      id: id,
      encoding: Keyword.get(opts, :encoding),
      fmtp: Keyword.get(opts, :fmtp)
    }
  end

  @doc """
  Generates stream id, that can be used to mark tracks belonging to the same stream.
  """
  @spec stream_id() :: String.t()
  def stream_id(), do: UUID.uuid4()
end
