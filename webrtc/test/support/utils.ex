defmodule Membrane.RTC.Engine.Support.Utils do
  @moduledoc false

  alias Membrane.Buffer
  alias Membrane.WebRTC.Track

  @spec generate_random_payload(non_neg_integer) :: binary()
  def generate_random_payload(size) when size >= 0 do
    for _i <- 1..size, into: <<>>, do: <<Enum.random(0..255)>>
  end

  @spec generator(any(), non_neg_integer()) :: {[Membrane.Element.Action.t()], any()}
  def generator(state, buffers_cnt) do
    payload = <<1, 2, 3, 4, 5>>
    metadata = %{is_keyframe: false}

    buffers =
      for _i <- 1..buffers_cnt do
        %Buffer{payload: payload, metadata: metadata}
      end

    {[buffer: {:output, buffers}], state}
  end

  @doc """
  Returns DTLS certificate fingerprint in binary form.
  """
  @spec get_cert_fingerprint() :: binary()
  def get_cert_fingerprint() do
    <<218, 225, 191, 40, 35, 120, 150, 183, 44, 117, 113, 254, 68, 136, 0, 164, 32, 0, 95, 220,
      113, 156, 179, 221, 80, 249, 148, 134, 26, 160, 116, 25>>
  end

  @doc """
  Creates new `t:Track.t/0`.
  """
  @spec get_track(:audio | :video) :: Track.t()
  def get_track(type \\ :audio) do
    stream_id = Track.stream_id()
    Track.new(type, stream_id)
  end
end
