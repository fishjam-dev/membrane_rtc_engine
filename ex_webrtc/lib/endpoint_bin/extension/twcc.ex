defmodule Membrane.WebRTC.Extension.TWCC do
  @moduledoc """
  Module implementing `Membrane.WebRTC.Extension` behaviour for Transport-wide Congestion Control RTP Header extension.

  This extension is described at https://datatracker.ietf.org/doc/html/draft-holmer-rmcat-transport-wide-cc-extensions-01.
  """
  @behaviour Membrane.WebRTC.Extension

  alias ExSDP.Attribute.{Extmap, RTCPFeedback}
  alias Membrane.WebRTC.Extension

  @name :twcc
  @uri "http://www.ietf.org/id/draft-holmer-rmcat-transport-wide-cc-extensions-01"
  @twcc_recv_rtp_module Membrane.RTP.TWCCReceiver
  @twcc_send_rtp_module Membrane.RTP.TWCCSender

  @impl true
  def new(opts \\ Keyword.new()),
    do: %Extension{module: __MODULE__, rtp_opts: opts, uri: @uri, name: @name}

  @impl true
  def compatible?(_encoding), do: true

  @impl true
  def get_rtp_module(twcc_id, _rtp_opts, :inbound), do: %@twcc_recv_rtp_module{twcc_id: twcc_id}
  def get_rtp_module(_twcc_id, _rtp_opts, :outbound), do: @twcc_send_rtp_module

  @impl true
  def add_to_media(media, id, _direction, payload_types) do
    media
    |> ExSDP.add_attribute(%Extmap{id: id, uri: @uri})
    |> ExSDP.add_attributes(Enum.map(payload_types, &%RTCPFeedback{pt: &1, feedback_type: :twcc}))
  end

  @impl true
  def uri, do: @uri
end
