defmodule Membrane.RTP.Opus.Depayloader do
  @moduledoc """
  Parses RTP payloads into parseable Opus packets.

  Based on [RFC 7587](https://tools.ietf.org/html/rfc7587).
  """

  use Membrane.Filter

  alias Membrane.{Opus, RemoteStream, RTP}

  def_input_pad :input, accepted_format: RTP, demand_mode: :auto

  def_output_pad :output,
    accepted_format: %RemoteStream{type: :packetized, content_format: Opus},
    demand_mode: :auto

  @impl true
  def handle_stream_format(:input, _stream_format, _context, state) do
    {
      [stream_format: {:output, %RemoteStream{type: :packetized, content_format: Opus}}],
      state
    }
  end

  @impl true
  def handle_process(:input, buffer, _ctx, state) do
    {[buffer: {:output, buffer}], state}
  end
end
