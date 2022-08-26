defmodule Membrane.RTC.Engine.Endpoint.WebRTC.Forwarder do
  @moduledoc false
  # Module responsible for forwarding RTP packets.
  # It takes care of rewriting RTP header and parts of RTP payload.

  require Membrane.Pad
  require Membrane.Logger

  alias Membrane.RTC.Engine.Endpoint.WebRTC.RTPMunger
  alias Membrane.RTC.Engine.Endpoint.WebRTC.VP8Munger

  @opaque t() :: %__MODULE__{
            codec: :H264 | :VP8,
            rtp_munger: RTPMunger.t(),
            vp8_munger: VP8Munger.t(),
            started?: boolean()
          }

  @enforce_keys [:codec, :rtp_munger]
  defstruct @enforce_keys ++
              [
                :vp8_munger,
                started?: false
              ]

  @doc """
  Creates a new forwarder.
  """
  @spec new(:H264 | :VP8, Membrane.RTP.clock_rate_t()) :: t()
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

  @spec reconfigure(t(), Membrane.Buffer.t()) :: t()
  def reconfigure(%__MODULE__{started?: false} = forwarder, buffer) do
    Membrane.Logger.debug("Initializing RTP and VP8 mungers")

    %__MODULE__{
      rtp_munger: rtp_munger,
      vp8_munger: vp8_munger
    } = forwarder

    rtp_munger = RTPMunger.init(rtp_munger, buffer)
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

    rtp_munger = RTPMunger.update(rtp_munger, buffer)

    vp8_munger =
      if vp8_munger do
        VP8Munger.update(vp8_munger, buffer)
      else
        vp8_munger
      end

    %__MODULE__{forwarder | rtp_munger: rtp_munger, vp8_munger: vp8_munger}
  end

  def process(
        %__MODULE__{started?: false} = forwarder,
        %{metadata: %{is_keyframe: true}} = buffer
      ) do
    forwarder = %{forwarder | started?: true}
    actions = [buffer: {:output, buffer}]
    {forwarder, actions}
  end

  def process(%__MODULE__{} = forwarder, buffer) do
    %__MODULE__{
      rtp_munger: rtp_munger,
      vp8_munger: vp8_munger
    } = forwarder

    {rtp_munger, buffer} = RTPMunger.munge(rtp_munger, buffer)

    {vp8_munger, buffer} =
      if vp8_munger do
        VP8Munger.munge(vp8_munger, buffer)
      else
        {vp8_munger, buffer}
      end

    forwarder = %{forwarder | rtp_munger: rtp_munger, vp8_munger: vp8_munger}
    actions = [buffer: {:output, buffer}]
    {forwarder, actions}
  end
end
