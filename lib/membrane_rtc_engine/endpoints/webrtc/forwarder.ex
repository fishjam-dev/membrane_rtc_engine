defmodule Membrane.RTC.Engine.Endpoint.WebRTC.Forwarder do
  @moduledoc false
  # Module responsible for forwarding RTP packets.
  # It takes care of rewriting RTP header and parts of RTP payload.

  require Membrane.Pad
  require Membrane.Logger

  alias Membrane.RTC.Engine.Endpoint
  alias Membrane.RTC.Engine.Endpoint.WebRTC.RTPMunger
  alias Membrane.RTC.Engine.Endpoint.WebRTC.VP8Munger

  @typedoc """
  * `selected_encoding` - encoding currently being forwarded.
  * `queued_encoding` - encoding forwarder will switch to once it receives a keyframe.
  * `old_encoding` - encoding that was used before becoming inactive. Once this encoding
  becomes active again, forwarder will switch back to it.
  """
  @opaque t() :: %__MODULE__{
            codec: :H264 | :VP8,
            selected_encoding: String.t() | nil,
            queued_encoding: String.t() | nil,
            old_encoding: String.t() | nil,
            rtp_munger: RTPMunger.t(),
            vp8_munger: VP8Munger.t(),
            encodings: [String.t()],
            active_encodings: [String.t()],
            started?: boolean()
          }

  @enforce_keys [:codec, :selected_encoding, :encodings, :active_encodings, :rtp_munger]
  defstruct @enforce_keys ++
              [
                :queued_encoding,
                :old_encoding,
                :vp8_munger,
                started?: false
              ]

  @doc """
  Creates a new forwarder.

  * `encodings` - a list of possible encodings.
  * `selected_encoding` - encoding to forward. If not provided,
  the highest possible encoding will be chosen.
  """
  @spec new(:H264 | :VP8, Membrane.RTP.clock_rate_t(), [String.t()], String.t() | nil) :: t()
  def new(codec, clock_rate, encodings, selected_encoding \\ nil)

  def new(:VP8, clock_rate, encodings, selected_encoding) do
    %__MODULE__{
      codec: :VP8,
      rtp_munger: RTPMunger.new(clock_rate),
      vp8_munger: VP8Munger.new(),
      encodings: encodings,
      # assume that, at the beginning, all encodings are active
      # if some encoding is inactive, we will be notified
      # about this in `encoding_inactive` function
      active_encodings: encodings,
      selected_encoding: selected_encoding || get_next_encoding(encodings)
    }
  end

  def new(:H264, clock_rate, encodings, selected_encoding) do
    %__MODULE__{
      codec: :H264,
      rtp_munger: RTPMunger.new(clock_rate),
      encodings: encodings,
      active_encodings: encodings,
      selected_encoding: selected_encoding || get_next_encoding(encodings)
    }
  end

  @doc """
  Marks given `encoding` as inactive.

  If given `encoding` is currently used it will be saved
  and forwarder will switch back to it once it becomes
  active again.

  Returns new forwarder and queued encoding or `nil` if
  forwarder doesn't need to switch at all.
  """
  @spec encoding_inactive(t(), String.t()) :: {t(), String.t() | nil}
  def encoding_inactive(forwarder, encoding) do
    forwarder = %__MODULE__{
      forwarder
      | active_encodings: List.delete(forwarder.active_encodings, encoding)
    }

    next_encoding = get_next_encoding(forwarder.active_encodings)

    cond do
      # if this is currently used and selected encoding
      forwarder.selected_encoding == encoding and forwarder.old_encoding == nil ->
        forwarder = %__MODULE__{forwarder | selected_encoding: nil, old_encoding: encoding}
        forwarder = select_encoding(forwarder, next_encoding)
        {forwarder, next_encoding}

      # if this is currently used encoding but it wasn't explicitly selected
      # i.e. we switched to it automatically
      forwarder.selected_encoding == encoding ->
        forwarder = %__MODULE__{forwarder | selected_encoding: nil}
        forwarder = select_encoding(forwarder, next_encoding)
        {forwarder, next_encoding}

      true ->
        {forwarder, nil}
    end
  end

  @doc """
  Marks given `encoding` as active.

  If this was the encoding used before it became inactive
  forwarder will switch back to it.

  Returns new forwarder and queued encoding or `nil` if
  forwarder doesn't need to switch at all.
  """
  @spec encoding_active(t(), String.t()) :: {t(), String.t() | nil}
  def encoding_active(forwarder, encoding) do
    forwarder = %__MODULE__{
      forwarder
      | active_encodings: forwarder.active_encodings ++ [encoding]
    }

    next_encoding = get_next_encoding(forwarder.active_encodings)

    cond do
      # if this is encoding we were using
      # before it became inactive
      forwarder.old_encoding == encoding ->
        forwarder = %__MODULE__{forwarder | old_encoding: nil}
        forwarder = select_encoding(forwarder, encoding)
        {forwarder, encoding}

      # if we are waiting for selected encoding to become
      # active again, try to select a new encoding
      # it might be better than currently used
      forwarder.old_encoding != nil ->
        forwarder = select_encoding(forwarder, next_encoding)
        {forwarder, next_encoding}

      # if we didn't have selected encoding either because
      # there aren't any active encodings or we are
      # waiting for a keyframe for queued encoding
      forwarder.selected_encoding == nil ->
        select_encoding(forwarder, next_encoding)
        {forwarder, next_encoding}

      true ->
        {forwarder, nil}
    end
  end

  @doc """
  Selects encoding forwarder will forward.
  """
  @spec select_encoding(forwarder :: t(), encoding :: String.t()) :: t()
  def select_encoding(forwarder, nil) do
    Membrane.Logger.debug("No active encoding.")
    %__MODULE__{forwarder | selected_encoding: nil, queued_encoding: nil}
  end

  def select_encoding(
        %__MODULE__{selected_encoding: encoding, queued_encoding: nil} = forwarder,
        encoding
      ) do
    Membrane.Logger.debug("Requested currently used encoding #{encoding}. Ignoring.")
    forwarder
  end

  def select_encoding(%__MODULE__{selected_encoding: encoding} = forwarder, encoding) do
    Membrane.Logger.debug("""
    Requested encoding: #{inspect(encoding)} which is currently used but while waiting
    for keyframe for queued_encoding #{inspect(forwarder.queued_encoding)}.
    Clearing queued encoding #{inspect(forwarder.queued_encoding)}
    """)

    %__MODULE__{forwarder | queued_encoding: nil}
  end

  def select_encoding(forwarder, encoding) do
    # enqueue encoding, we will switch when
    # we receive key frame
    Membrane.Logger.debug("Enqueuing encoding #{inspect(encoding)}.")
    %__MODULE__{forwarder | queued_encoding: encoding}
  end

  defp get_next_encoding(encodings) do
    encodings |> sort_encodings() |> List.first()
  end

  defp sort_encodings(encodings) do
    Enum.sort_by(
      encodings,
      fn
        "h" -> 3
        "m" -> 2
        "l" -> 1
      end,
      :desc
    )
  end

  @spec process(t(), Membrane.Buffer.t(), Endpoint.WebRTC.encoding_t()) ::
          {t(), [Membrane.Element.Action.t()]}
  def process(%__MODULE__{encodings: []} = forwarder, buffer, nil) do
    # don't modify non-simulacst packets
    {forwarder, [buffer: {:output, buffer}]}
  end

  def process(%__MODULE__{started?: false} = forwarder, buffer, encoding) do
    # init mungers with the first non-empty RTP packet
    %__MODULE__{
      queued_encoding: queued_encoding,
      rtp_munger: rtp_munger,
      vp8_munger: vp8_munger
    } = forwarder

    if buffer.payload != <<>> and encoding == queued_encoding and buffer.metadata.is_keyframe do
      Membrane.Logger.debug("Initializing RTP and VP8 mungers")
      rtp_munger = RTPMunger.init(rtp_munger, buffer)
      vp8_munger = if vp8_munger, do: VP8Munger.init(vp8_munger, buffer)

      forwarder = %{
        forwarder
        | rtp_munger: rtp_munger,
          vp8_munger: vp8_munger,
          selected_encoding: queued_encoding,
          queued_encoding: nil,
          started?: true
      }

      actions = [buffer: {:output, buffer}]
      {forwarder, actions}
    else
      {forwarder, []}
    end
  end

  def process(forwarder, buffer, encoding) do
    %__MODULE__{
      selected_encoding: selected_encoding,
      queued_encoding: queued_encoding,
      rtp_munger: rtp_munger,
      vp8_munger: vp8_munger
    } = forwarder

    cond do
      # we received packet for encoding we are going to switch to
      # and this packet represents a keyframe
      # update mungers and start forwarding new encoding
      queued_encoding == encoding and buffer.payload != <<>> and buffer.metadata.is_keyframe ->
        Membrane.Logger.info("""
        Received keyframe on requested encoding: #{inspect(queued_encoding)}. Switching.
        """)

        rtp_munger = RTPMunger.update(rtp_munger, buffer)
        {rtp_munger, buffer} = RTPMunger.munge(rtp_munger, buffer)

        {vp8_munger, buffer} =
          if vp8_munger do
            vp8_munger = VP8Munger.update(vp8_munger, buffer)
            VP8Munger.munge(vp8_munger, buffer)
          else
            {vp8_munger, buffer}
          end

        forwarder = %{
          forwarder
          | rtp_munger: rtp_munger,
            vp8_munger: vp8_munger,
            selected_encoding: queued_encoding,
            queued_encoding: nil
        }

        actions = [buffer: {:output, buffer}]
        {forwarder, actions}

      # we received packet for encoding we are currently forwarding
      # munge and forward it
      selected_encoding == encoding ->
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

      true ->
        {forwarder, []}
    end
  end
end
