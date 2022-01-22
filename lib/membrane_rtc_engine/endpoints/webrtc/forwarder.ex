defmodule Membrane.RTC.Engine.Endpoint.WebRTC.Forwarder do
  @moduledoc false
  # Module responsible for forwarding RTP packets.
  # It takes care of rewriting RTP header and parts of RTP payload.

  alias Membrane.Pad
  alias Membrane.RTC.Engine.Endpoint
  alias Membrane.RTC.Engine.Endpoint.WebRTC.RTPMunger
  alias Membrane.RTC.Engine.Endpoint.WebRTC.VP8Munger
  alias Membrane.RTC.Engine.Endpoint.WebRTC.Utils

  require Membrane.Pad
  require Membrane.Logger

  @typedoc """
  * `selected_encoding` - encoding currently being forwarded.
  * `queued_encoding` - encoding forwarder will switch to once it receives a keyframe.
  * `old_encoding` - encoding that was used before becoming inactive. Once this encoding
  becomes active again, forwarder will switch back to it.
  """
  @type t() :: %__MODULE__{
          selected_encoding: String.t() | nil,
          queued_encoding: String.t() | nil,
          old_encoding: String.t() | nil,
          rtp_munger: RTPMunger.t(),
          vp8_munger: VP8Munger.t(),
          active_encodings: [String.t()],
          started?: boolean()
        }

  @enforce_keys [
    :selected_encoding,
    :queued_encoding,
    :old_encoding,
    :rtp_munger,
    :vp8_munger,
    :active_encodings,
    :started?
  ]
  defstruct @enforce_keys

  @doc """
  Creates a new forwarder.

  Clock rate has to be in Hz.
  """
  @spec new(non_neg_integer()) :: t()
  def new(clock_rate) do
    %__MODULE__{
      selected_encoding: "h",
      queued_encoding: nil,
      old_encoding: nil,
      rtp_munger: RTPMunger.new(clock_rate),
      vp8_munger: VP8Munger.new(),
      active_encodings: ["h", "l"],
      started?: false
    }
  end

  @spec encoding_inactive(t(), String.t()) :: t()
  def encoding_inactive(forwarder, encoding) do
    forwarder = %__MODULE__{
      forwarder
      | old_encoding: encoding,
        active_encodings: List.delete(forwarder.active_encodings, encoding)
    }

    do_select_encoding(forwarder, List.first(forwarder.active_encodings))
  end

  @spec encoding_active(t(), String.t()) :: t()
  def encoding_active(forwarder, encoding) do
    forwarder = %__MODULE__{
      forwarder
      | active_encodings: forwarder.active_encodings ++ [encoding]
    }

    cond do
      # if this is encoding we were using
      # before it became inactive
      forwarder.old_encoding == encoding ->
        forwarder = %__MODULE__{forwarder | old_encoding: nil}
        do_select_encoding(forwarder, encoding)

      # if we don't have any active encoding
      forwarder.selected_encoding == nil ->
        do_select_encoding(forwarder, List.first(forwarder.active_encodings))

      true ->
        forwarder
    end
  end

  @spec select_encoding(t(), String.t()) :: t()
  def select_encoding(forwarder, encoding) do
    do_select_encoding(forwarder, encoding)
  end

  defp do_select_encoding(forwarder, nil) do
    %__MODULE__{forwarder | selected_encoding: nil, queued_encoding: nil}
  end

  defp do_select_encoding(%__MODULE__{selected_encoding: encoding} = forwarder, encoding) do
    # if we want to switch to already selected encoding just
    # clear queued_encoding - this can happen in the following scenario
    #
    # selected_encoding: h
    # select encoding l -> selected_encoding: h, queued_encoding: l
    #
    # we haven't received keyframe for l encoding yet but we want to
    # switch back to h encoding again
    #
    # select encoding h -> selected_encoding: h, queued_encoding: nil
    %__MODULE__{forwarder | queued_encoding: nil}
  end

  defp do_select_encoding(forwarder, encoding) do
    # enqueue encoding, we will switch when
    # we receive key frame
    Membrane.Logger.info("Enqueuing encoding #{inspect(encoding)}.")
    %__MODULE__{forwarder | queued_encoding: encoding}
  end

  @spec process(t(), Membrane.Buffer.t(), Endpoint.WebRTC.encoding_t(), any()) ::
          {t(), [Membrane.Element.Action.t()]}
  def process(forwarder, buffer, encoding, endpoint_id) do
    %__MODULE__{
      selected_encoding: selected_encoding,
      queued_encoding: queued_encoding,
      rtp_munger: rtp_munger,
      vp8_munger: vp8_munger,
      started?: started?
    } = forwarder

    cond do
      # if we received first packet of the encoding we are
      # going to forward and this is not RTP padding only packet
      started? == false and selected_encoding == encoding and buffer.payload != <<>> ->
        rtp_munger = RTPMunger.init(rtp_munger, buffer)
        vp8_munger = VP8Munger.init(vp8_munger, buffer)

        forwarder = %{
          forwarder
          | rtp_munger: rtp_munger,
            vp8_munger: vp8_munger,
            selected_encoding: encoding,
            started?: true
        }

        actions = [buffer: {Pad.ref(:output, {:endpoint, endpoint_id}), buffer}]
        {forwarder, actions}

      queued_encoding == encoding and buffer.payload != <<>> ->
        if Utils.is_vp8_keyframe(buffer.payload) do
          Membrane.Logger.info("""
          Received keyframe on requested encoding: #{inspect(queued_encoding)}. Switching.
          """)

          rtp_munger = RTPMunger.update(rtp_munger, buffer)
          vp8_munger = VP8Munger.update(vp8_munger, buffer)

          {rtp_munger, buffer} = RTPMunger.munge(rtp_munger, buffer)
          {vp8_munger, buffer} = VP8Munger.munge(vp8_munger, buffer)

          forwarder = %{
            forwarder
            | rtp_munger: rtp_munger,
              vp8_munger: vp8_munger,
              selected_encoding: queued_encoding,
              queued_encoding: nil
          }

          actions = [
            notify: {:encoding_switched, endpoint_id, encoding},
            buffer: {Pad.ref(:output, {:endpoint, endpoint_id}), buffer}
          ]

          {forwarder, actions}
        else
          {forwarder, []}
        end

      selected_encoding == encoding ->
        {rtp_munger, buffer} = RTPMunger.munge(rtp_munger, buffer)
        {vp8_munger, buffer} = VP8Munger.munge(vp8_munger, buffer)

        forwarder = %{forwarder | rtp_munger: rtp_munger, vp8_munger: vp8_munger}
        actions = [buffer: {Pad.ref(:output, {:endpoint, endpoint_id}), buffer}]
        {forwarder, actions}

      true ->
        {forwarder, []}
    end
  end
end
