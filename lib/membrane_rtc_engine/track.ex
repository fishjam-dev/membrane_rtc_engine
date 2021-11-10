defmodule Membrane.RTC.Engine.Track do
  @moduledoc """
  Module representing media track.

  Media track is a single audio or video. Tracks that are related to each other
  (e.g. audio from microphone that corresponds to video from a web cam) can be grouped into the same stream by
  assigning each of them the same stream id.
  """
  alias ExSDP.Attribute.FMTP

  @enforce_keys [:type, :stream_id, :id, :fmtp]
  defstruct @enforce_keys ++ [encoding: nil, format: nil, active?: true]

  @type id :: String.t()
  @type encoding :: atom()
  @type format :: [atom()]

  @typedoc """
  This module contains:
  * `type` - audio or video,
  * `stream_id` - media stream this track belongs to. Relationship between tracks (e.g. audio and video)
  can be indicated by assigning each of them the same `stream_id`. One `stream_id` can be assign to any
  number of tracks.
  * `id` - track id
  * `encoding` - track encoding
  * `format` - list of available track formats. At this moment max two formats can be specified.
  One of them has to be `:raw` which indicates that other Endpoints will receive this track in format
  of `encoding`. The other one can be any atom (e.g. `:RTP`).
  * `fmtp` - struct describing format specific parameters e.g. for H264 it contains `profile_level_id`
  * `active?` - indicates whether track is still available or not (because peer left a room)
  """
  @type t :: %__MODULE__{
          type: :audio | :video,
          stream_id: String.t(),
          id: id,
          encoding: encoding,
          format: format,
          fmtp: FMTP,
          active?: boolean()
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
