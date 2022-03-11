# credo:disable-for-this-file Credo.Check.Design.TagTODO
defmodule Membrane.RTC.Engine.Endpoint.WebRTC.RTPMunger do
  @moduledoc false
  # Module responsible for rewriting RTP packet's sequence number and timestamp
  # to provide transparent switch between simulcast encodings.
  #
  # It is almost rewritten from livekit
  # https://github.com/livekit/livekit-server/blob/90b97137c9f36da2136291f738d83146f3faaf66/pkg/sfu/rtpmunger.go#L86
  #
  # The main difference is that we do not ignore RTP padding packets sent
  # by browser to probe the connection (estimate bandwidth) or to satisfy minimal bandwidth requirement.
  # It is unclear what we should do so we chose the easiest solution i.e.
  # we simply pass them to the other side.
  use Bitwise

  @typedoc """
  * `highest_incoming_seq_num` - the highest incoming sequence number for current encoding.
    It does not include `seq_num_offset` so unlike `last_seq_num`,
    it is different for different encodings (it is not contiguous)
  * `last_seq_num` - last sequence number we received. It includes `seq_num_offset`
    so it is contiguous until rollover
  """
  @type t() :: %__MODULE__{
          clock_rate: non_neg_integer(),
          highest_incoming_seq_num: integer(),
          last_seq_num: integer(),
          seq_num_offset: integer(),
          last_timestamp: integer(),
          timestamp_offset: integer(),
          last_packet_arrival: integer()
        }

  @enforce_keys [
    :clock_rate,
    :highest_incoming_seq_num,
    :last_seq_num,
    :seq_num_offset,
    :last_timestamp,
    :timestamp_offset,
    :last_packet_arrival
  ]
  defstruct @enforce_keys

  @doc """
  Creates new RTP munger.

  Clock rate has to be in Hz.
  """
  @spec new(non_neg_integer()) :: t()
  def new(clock_rate) do
    %__MODULE__{
      clock_rate: clock_rate,
      highest_incoming_seq_num: 0,
      last_seq_num: 0,
      seq_num_offset: 0,
      last_timestamp: 0,
      timestamp_offset: 0,
      last_packet_arrival: 0
    }
  end

  @spec init(t(), Membrane.Buffer.t()) :: t()
  def init(rtp_munger, buffer) do
    %__MODULE__{
      rtp_munger
      | highest_incoming_seq_num: buffer.metadata.rtp.sequence_number - 1,
        last_seq_num: buffer.metadata.rtp.sequence_number,
        last_timestamp: buffer.metadata.rtp.timestamp,
        last_packet_arrival: System.monotonic_time(:millisecond)
    }
  end

  @spec update(t(), Membrane.Buffer.t()) :: t()
  def update(rtp_munger, buffer) do
    arrival = System.monotonic_time(:millisecond)
    # convert clock rate 1/s to 1/ms
    clock_rate = round(rtp_munger.clock_rate / 1000)
    adj = (arrival - rtp_munger.last_packet_arrival) * clock_rate

    # if packet in an encoding we are going to switch to arrived
    # exactly or almost at the same time
    # as the last packet in currently used encoding
    # then set adjustment to one so that we do not send
    # two packets with the same timestamp
    adj = if adj == 0, do: 1, else: adj
    timestamp_offset = buffer.metadata.rtp.timestamp - rtp_munger.last_timestamp - adj
    seq_num_offset = buffer.metadata.rtp.sequence_number - rtp_munger.last_seq_num - 1

    %__MODULE__{
      rtp_munger
      | highest_incoming_seq_num: buffer.metadata.rtp.sequence_number - 1,
        seq_num_offset: seq_num_offset,
        timestamp_offset: timestamp_offset
    }
  end

  @spec munge(t(), Membrane.Buffer.t()) :: {t(), Membrane.Buffer.t()}
  def munge(rtp_munger, buffer) do
    # TODO we should use Sender Reports instead
    packet_arrival = System.monotonic_time(:millisecond)

    update_sn_ts = fn buffer ->
      metadata =
        buffer.metadata
        |> update_in([:rtp, :sequence_number], fn seq_num ->
          # add 1 <<< 16 (max sequence number) to handle sequence number rollovers
          # properly - we will avoid negative sequence numbers in this way
          rem(seq_num + (1 <<< 16) - rtp_munger.seq_num_offset, 1 <<< 16)
        end)
        |> update_in([:rtp, :timestamp], fn timestamp ->
          rem(timestamp + (1 <<< 32) - rtp_munger.timestamp_offset, 1 <<< 32)
        end)

      %Membrane.Buffer{buffer | metadata: metadata}
    end

    seq_num_diff = buffer.metadata.rtp.sequence_number - rtp_munger.highest_incoming_seq_num

    cond do
      # out-of-order packet - update its sequence number
      # and timestamp without updating munger
      seq_num_diff > -(1 <<< 15) and seq_num_diff < 0 ->
        buffer = update_sn_ts.(buffer)
        {rtp_munger, buffer}

      # duplicate packet
      # at the moment, perform the same actions we are performing
      # for out-of-order packet
      # TODO maybe we should ignore it?
      seq_num_diff == 0 ->
        buffer = update_sn_ts.(buffer)
        {rtp_munger, buffer}

      # in order but not necessarily contiguous packet
      true ->
        highest_incoming_seq_num = buffer.metadata.rtp.sequence_number
        buffer = update_sn_ts.(buffer)

        rtp_munger = %__MODULE__{
          rtp_munger
          | highest_incoming_seq_num: highest_incoming_seq_num,
            last_seq_num: buffer.metadata.rtp.sequence_number,
            last_timestamp: buffer.metadata.rtp.timestamp,
            last_packet_arrival: packet_arrival
        }

        {rtp_munger, buffer}
    end
  end
end
