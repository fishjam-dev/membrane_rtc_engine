defmodule Membrane.RTC.Engine.Track do
  @moduledoc """
  Module representing media track.

  Media track is a single audio or video. Tracks that are related to each other
  (e.g. audio from microphone that corresponds to video from a web cam) can be grouped into the same stream by
  assigning each of them the same stream id.
  """
  alias ExSDP.Attribute.FMTP

  @enforce_keys [:type, :stream_id, :id, :fmtp]
  defstruct @enforce_keys ++ [encoding: nil, format: nil, disabled?: false]

  @type id :: String.t()
  @type encoding :: :OPUS | :H264 | :VP8
  @type format :: atom()

  @typedoc """
  This module contains:
  type - track can be of type audio or video,
  stream_id - track can belong to some media stream. Each media stream can contains multiple tracks. This allow to group tracks
  id - allow to identify track
  encoding -  describe in what codec media is encodec e.g. OPUS, VP8
  format - describe in what format media is sent e.g. raw, RTP, RTMP
  fmtp - struct describing format specific parameters e.g. for H264 it contains profile_level_id
  disabled? - describe if track is still enabld (peer is still in room and sending media)
  """
  @type t :: %__MODULE__{
          type: :audio | :video,
          stream_id: String.t(),
          id: id,
          encoding: encoding,
          format: format,
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
          format: format,
          fmtp: FMTP
        ) :: t
  def new(type, stream_id, opts \\ []) do
    id = Keyword.get(opts, :id, Base.encode16(:crypto.strong_rand_bytes(8)))

    %__MODULE__{
      type: type,
      stream_id: stream_id,
      id: id,
      encoding: Keyword.get(opts, :encoding),
      format: Keyword.get(opts, :format),
      fmtp: Keyword.get(opts, :fmtp)
    }
  end

  @doc """
  Generates stream id, that can be used to mark tracks belonging to the same stream.
  """
  @spec stream_id() :: String.t()
  def stream_id(), do: UUID.uuid4()
end
