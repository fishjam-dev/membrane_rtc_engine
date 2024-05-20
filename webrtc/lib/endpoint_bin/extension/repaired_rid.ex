defmodule Membrane.WebRTC.Extension.RepairedRid do
  @moduledoc """
  Module implementing `Membrane.WebRTC.Extension` behaviour for Repaired RTP Stream Identifier RTP Header extension.

  This extension is described in RFC 8852.
  """
  @behaviour Membrane.WebRTC.Extension

  alias ExSDP.Attribute.Extmap
  alias Membrane.WebRTC.Extension

  @name :repaired_rid
  @uri "urn:ietf:params:rtp-hdrext:sdes:repaired-rtp-stream-id"

  @impl true
  def new(opts \\ Keyword.new()),
    do: %Extension{module: __MODULE__, rtp_opts: opts, uri: @uri, name: @name}

  @impl true
  def compatible?(_encoding), do: true

  @impl true
  def get_rtp_module(_rid_id, _opts, _track_type), do: :no_rtp_module

  @impl true
  def add_to_media(media, id, _direction, _payload_types),
    do: ExSDP.add_attribute(media, %Extmap{id: id, uri: @uri})

  @impl true
  def uri, do: @uri
end
