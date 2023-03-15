defmodule Membrane.RTC.Engine.Endpoint.WebRTC.Forwarder do
  @moduledoc false
  # Module responsible for forwarding RTP packets.
  # It takes care of rewriting RTP header and parts of RTP payload.

  require Membrane.Pad
  require Membrane.Logger

  alias Membrane.Buffer
  alias Membrane.RTC.Engine.Endpoint.WebRTC.RTPMunger
  alias Membrane.RTC.Engine.Endpoint.WebRTC.VP8Munger
  alias Membrane.RTC.Engine.Track

  @opaque t() :: %__MODULE__{
            codec: :H264 | :VP8 | :OPUS,
            rtp_munger: RTPMunger.t(),
            vp8_munger: VP8Munger.t(),
            started?: boolean()
          }

  @enforce_keys [:codec]
  defstruct @enforce_keys ++
              [
                :rtp_munger,
                :vp8_munger,
                started?: false
              ]

  @doc """
  Creates a new forwarder.
  """
  @spec new(:H264 | :VP8 | :OPUS, Membrane.RTP.clock_rate_t()) :: t()
  def new(codec, clock_rate)

  def new(:VP8, clock_rate) do
    %__MODULE__{
      codec: :VP8,
      rtp_munger: RTPMunger.new(clock_rate),
      vp8_munger: VP8Munger.new()
    }
  end

  def new(:H264, clock_rate) do
    %__MODULE__{
      codec: :H264,
      rtp_munger: RTPMunger.new(clock_rate)
    }
  end

  def new(:OPUS, _clock_rate) do
    %__MODULE__{codec: :OPUS}
  end

  @doc """
  Reconfigures forwarder after encoding switch.
  """
  @spec reconfigure(t(), Membrane.Buffer.t()) :: t()
  def reconfigure(%__MODULE__{started?: false} = forwarder, buffer) do
    Membrane.Logger.debug("Initializing RTP and VP8 mungers")

    %__MODULE__{
      rtp_munger: rtp_munger,
      vp8_munger: vp8_munger
    } = forwarder

    rtp_munger = if rtp_munger, do: RTPMunger.init(rtp_munger, buffer)
    vp8_munger = if vp8_munger, do: VP8Munger.init(vp8_munger, buffer)

    %{
      forwarder
      | rtp_munger: rtp_munger,
        vp8_munger: vp8_munger
    }
  end

  def reconfigure(%__MODULE__{} = forwarder, buffer) do
    Membrane.Logger.debug("Reconfiguring forwader with buffer: #{inspect(buffer)}")

    %__MODULE__{
      rtp_munger: rtp_munger,
      vp8_munger: vp8_munger
    } = forwarder

    rtp_munger = if rtp_munger, do: RTPMunger.update(rtp_munger, buffer)
    vp8_munger = if vp8_munger, do: VP8Munger.update(vp8_munger, buffer)

    %__MODULE__{forwarder | rtp_munger: rtp_munger, vp8_munger: vp8_munger}
  end

  @spec generate_padding_packet(t(), Track.t(), boolean()) :: {t(), Buffer.t() | nil}
  def generate_padding_packet(%__MODULE__{} = forwarder, %Track{} = track, force_marker?) do
    {rtp_munger, buffer} =
      RTPMunger.generate_padding_packet(forwarder.rtp_munger, track, force_marker?)

    {%{forwarder | rtp_munger: rtp_munger}, buffer}
  end

  @spec can_generate_padding_packet?(t()) :: boolean()
  def can_generate_padding_packet?(%__MODULE__{} = forwarder) do
    RTPMunger.can_generate_padding_packet?(forwarder.rtp_munger)
  end

  @doc """
  Adjusts RTP packet header and payload.
  """
  @spec align(t(), Buffer.t()) :: {t(), Buffer.t() | nil}
  def align(
        %__MODULE__{started?: false} = forwarder,
        %{metadata: %{is_keyframe: true}} = buffer
      ) do
    forwarder = %{forwarder | started?: true}
    {forwarder, buffer}
  end

  def align(%__MODULE__{} = forwarder, buffer) do
    %__MODULE__{
      rtp_munger: rtp_munger,
      vp8_munger: vp8_munger
    } = forwarder

    {rtp_munger, buffer} =
      if rtp_munger do
        RTPMunger.munge(rtp_munger, buffer)
      else
        {rtp_munger, buffer}
      end

    {vp8_munger, buffer} =
      if vp8_munger != nil and buffer != nil do
        VP8Munger.munge(vp8_munger, buffer)
      else
        {vp8_munger, buffer}
      end

    forwarder = %{forwarder | rtp_munger: rtp_munger, vp8_munger: vp8_munger}
    {forwarder, buffer}
  end
end
