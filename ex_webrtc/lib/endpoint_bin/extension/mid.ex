defmodule Membrane.WebRTC.Extension.Mid do
  @moduledoc """
  Module implementing `Membrane.WebRTC.Extension` behaviour for Media Identification RTP Header extension.

  This extension is described at https://tools.ietf.org/pdf/draft-ietf-mmusic-rfc8843bis-09.pdf.
  """
  @behaviour Membrane.WebRTC.Extension
  alias ExSDP.Attribute.Extmap
  alias Membrane.WebRTC.Extension

  @name :mid
  @uri "urn:ietf:params:rtp-hdrext:sdes:mid"

  @impl true
  def new(opts \\ Keyword.new()),
    do: %Extension{
      module: __MODULE__,
      rtp_opts: opts,
      uri: @uri,
      name: @name
    }

  @impl true
  def compatible?(_encoding), do: true

  # there is no module parsing RTP headers against this extension as
  # for the whole session mid for same buffer will be the same.
  # It is used only in handler for `:new_rtp_stream` notification.
  @impl true
  def get_rtp_module(_mid_id, _opts, _track_type), do: :no_rtp_module

  @impl true
  def add_to_media(media, id, _direction, _payload_types),
    do: ExSDP.add_attribute(media, %Extmap{id: id, uri: @uri})

  @impl true
  def uri, do: @uri
end
