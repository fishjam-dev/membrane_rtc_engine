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
  import Bitwise

  alias __MODULE__.Cache
  alias Membrane.RTC.Engine.Track

  @typedoc """
  * `highest_incoming_seq_num` - the highest incoming sequence number for current encoding.
    It does not include `seq_num_offset` so unlike `last_seq_num`,
    it is different for different encodings (it is not contiguous)
  * `last_seq_num` - last sequence number we received. It includes `seq_num_offset`
    so it is contiguous until rollover
  """
  @type t() :: %__MODULE__{
          clock_rate: Membrane.RTP.clock_rate_t(),
          cache: Cache.t(),
          highest_incoming_seq_num: integer(),
          last_seq_num: integer(),
          seq_num_offset: integer(),
          last_timestamp: integer(),
          last_marker: boolean(),
          timestamp_offset: integer(),
          last_packet_arrival: integer()
        }

  @enforce_keys [:clock_rate]
  defstruct @enforce_keys ++
              [
                cache: Cache.new(),
                highest_incoming_seq_num: 0,
                last_seq_num: 0,
                seq_num_offset: 0,
                last_timestamp: 0,
                timestamp_offset: 0,
                last_marker: true,
                last_packet_arrival: 0
              ]

  @doc """
  Creates new RTP munger.
  """
  @spec new(Membrane.RTP.clock_rate_t()) :: t()
  def new(clock_rate) do
    %__MODULE__{clock_rate: clock_rate}
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
        timestamp_offset: timestamp_offset,
        last_marker: true,
        cache: Cache.new()
    }
  end

  @spec generate_padding_packet(t(), Track.t()) :: {t(), Membrane.Buffer.t() | nil}
  def generate_padding_packet(rtp_munger, track) when rtp_munger.last_marker do
    # We can only generate padding packets at frame boundary
    buffer = %Membrane.Buffer{
      payload: <<>>,
      metadata: %{
        rtp: %{
          is_padding?: true,
          ssrc: "",
          extensions: [],
          csrcs: [],
          payload_type: track.payload_type,
          marker: false,
          sequence_number: calculate_seq_num(rtp_munger.highest_incoming_seq_num + 1, rtp_munger),
          timestamp: rem(rtp_munger.last_timestamp + (1 <<< 32) - 100_000, 1 <<< 32)
        }
      }
    }

    rtp_munger =
      rtp_munger
      |> Map.update!(:seq_num_offset, &(&1 - 1))
      |> Map.put(:last_seq_num, buffer.metadata.rtp.sequence_number)
      |> Map.update!(
        :cache,
        &Cache.remove_outdated_entries(&1, buffer.metadata.rtp.sequence_number)
      )

    {rtp_munger, buffer}
  end

  def generate_padding_packet(rtp_munger, _track), do: {rtp_munger, nil}

  @spec munge(t(), Membrane.Buffer.t()) :: {t(), Membrane.Buffer.t() | nil}
  def munge(rtp_munger, buffer) do
    # TODO we should use Sender Reports instead
    packet_arrival = System.monotonic_time(:millisecond)

    update_sn_ts = fn buffer ->
      metadata =
        buffer.metadata
        |> update_in([:rtp, :sequence_number], &calculate_seq_num(&1, rtp_munger))
        |> update_in([:rtp, :timestamp], &calculate_timestamp(&1, rtp_munger))

      %Membrane.Buffer{buffer | metadata: metadata}
    end

    seq_num_diff = buffer.metadata.rtp.sequence_number - rtp_munger.highest_incoming_seq_num

    if seq_num_diff > -(1 <<< 15) and seq_num_diff <= 0 do
      # out-of-order - update its sequence number
      # and timestamp without updating munger
      # 1 <<< 15 represents half of maximal sequence number
      # so we detect out-of-order packet when the difference is
      # high enough (-32 768; 0)
      #
      # to understand this more consider sequence number rollover
      # scenario in which the difference between subsequent sequence numbers
      # is equal to 0 - 65 536 = -65 536 - such packet cannot be
      # considered as out-of-order
      case Cache.get_and_remove(rtp_munger.cache, buffer.metadata.rtp.sequence_number) do
        {:ok, seq_num, cache} ->
          rtp_munger = %{rtp_munger | cache: cache}

          metadata =
            buffer.metadata
            |> update_in([:rtp, :timestamp], &calculate_timestamp(&1, rtp_munger))
            |> put_in([:rtp, :sequence_number], seq_num)

          {rtp_munger, %{buffer | metadata: metadata}}

        {:error, :not_found} ->
          {rtp_munger, nil}
      end
    else
      # in order but not necessarily contiguous packet
      highest_incoming_seq_num = buffer.metadata.rtp.sequence_number
      buffer = update_sn_ts.(buffer)

      cache =
        if seq_num_diff > 1 do
          (rtp_munger.highest_incoming_seq_num + 1)..(highest_incoming_seq_num - 1)
          |> Enum.reduce(rtp_munger.cache, fn seq_num, cache ->
            Cache.push(cache, seq_num, calculate_seq_num(seq_num, rtp_munger))
          end)
        else
          Cache.remove_outdated_entries(rtp_munger.cache, buffer.metadata.rtp.sequence_number)
        end

      rtp_munger = %__MODULE__{
        rtp_munger
        | highest_incoming_seq_num: highest_incoming_seq_num,
          last_seq_num: buffer.metadata.rtp.sequence_number,
          last_timestamp: buffer.metadata.rtp.timestamp,
          last_marker: buffer.metadata.rtp.marker,
          last_packet_arrival: packet_arrival,
          cache: cache
      }

      {rtp_munger, buffer}
    end
  end

  defp calculate_seq_num(seq_num, rtp_munger) do
    # add 1 <<< 16 (max sequence number) to handle sequence number rollovers
    # properly - we will avoid negative sequence numbers in this way
    rem(seq_num + (1 <<< 16) - rtp_munger.seq_num_offset, 1 <<< 16)
  end

  defp calculate_timestamp(timestamp, rtp_munger) do
    rem(timestamp + (1 <<< 32) - rtp_munger.timestamp_offset, 1 <<< 32)
  end
end
