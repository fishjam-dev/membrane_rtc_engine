defmodule Membrane.RTP.InboundPacketTracker do
  @moduledoc """
  Module responsible for tracking statistics of incoming RTP packets for a single stream.

  Tracker is capable of repairing packets' sequence numbers provided that it has information about how many packets has
  been previously discarded. To updated number of discarded packets one should send an event `Membrane.RTP.PacketsDiscarded.t/0` that will accumulate
  the total number of discarded packets and will subtract that number from the packet's sequence number.
  """
  use Membrane.Filter

  require Bitwise
  require Membrane.Logger

  alias Membrane.RTCP.ReceiverReport
  alias Membrane.RTP.{RetransmissionRequestEvent, SequenceNumberTracker}
  alias Membrane.{Buffer, RTP, Time}

  @max_diff 9000

  @max_seq_num Bitwise.bsl(1, 16) - 1
  @max_s24_val Bitwise.bsl(1, 23) - 1
  @min_s24_val -Bitwise.bsl(1, 23)

  def_input_pad :input, accepted_format: _any, demand_mode: :auto
  def_output_pad :output, accepted_format: _any, demand_mode: :auto

  def_options clock_rate: [
                spec: Membrane.RTP.clock_rate_t()
              ],
              repair_sequence_numbers?: [
                spec: boolean(),
                default: true,
                description: "Defines if tracker should try to repair packet's sequence number"
              ]

  defmodule State do
    @moduledoc false

    @type t :: %__MODULE__{
            clock_rate: non_neg_integer(),
            repair_sequence_numbers?: boolean(),
            seq_num_tracker: SequenceNumberTracker.t(),
            jitter: float(),
            transit: non_neg_integer() | nil,
            received: non_neg_integer(),
            discarded: non_neg_integer(),
            base_seq: non_neg_integer(),
            received_prior: non_neg_integer(),
            expected_prior: non_neg_integer(),
            lost: non_neg_integer(),
            fraction_lost: float()
          }

    @enforce_keys [:clock_rate, :repair_sequence_numbers?]
    defstruct @enforce_keys ++
                [
                  seq_num_tracker: SequenceNumberTracker.new(),
                  jitter: 0.0,
                  transit: nil,
                  received: 0,
                  discarded: 0,
                  base_seq: nil,
                  received_prior: 0,
                  expected_prior: 0,
                  lost: 0,
                  fraction_lost: 0.0
                ]
  end

  @impl true
  def handle_init(_ctx, opts) do
    {[],
     %State{clock_rate: opts.clock_rate, repair_sequence_numbers?: opts.repair_sequence_numbers?}}
  end

  @impl true
  def handle_process(:input, buffer, _ctx, %State{} = state) do
    seq_num = buffer.metadata.rtp.sequence_number

    {delta, packet_index, tracker} = SequenceNumberTracker.track(state.seq_num_tracker, seq_num)

    if abs(delta) > @max_diff do
      # The gap is too big, we consider this packet to be malformed
      # Ignore it
      Membrane.Logger.warn(
        "Dropping packet #{seq_num} with big sequence number difference (#{delta})"
      )

      # Do not update state, malformed packet could corrupt it
      {[], state}
    else
      state =
        %State{state | base_seq: state.base_seq || packet_index, seq_num_tracker: tracker}
        |> update_received()

      lost_ids = Enum.to_list((packet_index - (delta - 1))..(packet_index - 1)//1)

      actions = [buffer: {:output, repair_sequence_number(buffer, state)}]
      state = update_jitter(state, buffer)

      if Enum.empty?(lost_ids) do
        {actions, state}
      else
        {actions ++ [event: {:input, %RetransmissionRequestEvent{packet_ids: lost_ids}}], state}
      end
    end
  end

  @impl true
  def handle_event(:input, %ReceiverReport.StatsRequestEvent{}, _ctx, state) do
    %State{
      received: received,
      received_prior: received_prior,
      expected_prior: expected_prior,
      jitter: jitter,
      seq_num_tracker: %SequenceNumberTracker{highest_seen_index: highest_seq_num}
    } = state

    expected = expected_packets(state)

    lost = max(expected - received, 0)

    expected_interval = expected - expected_prior
    received_interval = received - received_prior

    lost_interval = expected_interval - received_interval

    fraction_lost =
      if expected_interval == 0 || lost_interval <= 0 do
        0.0
      else
        lost_interval / expected_interval
      end

    total_lost =
      cond do
        lost > @max_s24_val -> @max_s24_val
        lost < @min_s24_val -> @min_s24_val
        true -> lost
      end

    state = %State{
      state
      | expected_prior: expected,
        received_prior: received,
        lost: total_lost,
        fraction_lost: fraction_lost
    }

    stats = %ReceiverReport.Stats{
      fraction_lost: fraction_lost,
      total_lost: total_lost,
      highest_seq_num: highest_seq_num,
      interarrival_jitter: jitter
    }

    {[event: {:input, %ReceiverReport.StatsEvent{stats: stats}}], state}
  end

  @impl true
  def handle_event(
        :input,
        %RTP.PacketsDiscardedEvent{discarded: packets_discarded},
        _ctx,
        %State{discarded: discarded} = state
      ) do
    {[], %State{state | discarded: discarded + packets_discarded}}
  end

  @impl true
  def handle_event(direction, event, ctx, state), do: super(direction, event, ctx, state)

  defp update_received(%State{received: received} = state) do
    %State{state | received: received + 1}
  end

  defp expected_packets(%State{
         base_seq: base_seq,
         seq_num_tracker: %SequenceNumberTracker{highest_seen_index: max_idx}
       }) do
    max_idx - base_seq + 1
  end

  defp update_jitter(state, %Buffer{metadata: metadata}) do
    %State{clock_rate: clock_rate, jitter: last_jitter, transit: last_transit} = state

    # Algorithm from https://tools.ietf.org/html/rfc3550#appendix-A.8
    arrival_ts = Map.get(metadata, :arrival_ts, Time.vm_time())
    buffer_ts = metadata.rtp.timestamp
    arrival = arrival_ts |> Time.as_seconds() |> Ratio.mult(clock_rate) |> Ratio.trunc()
    transit = arrival - buffer_ts

    {jitter, transit} =
      if last_transit == nil do
        {last_jitter, transit}
      else
        d = abs(transit - last_transit)
        new_jitter = last_jitter + 1 / 16 * (d - last_jitter)

        {new_jitter, transit}
      end

    %State{state | jitter: jitter, transit: transit}
  end

  defp repair_sequence_number(%Buffer{} = buffer, %State{
         discarded: discarded,
         repair_sequence_numbers?: repair?
       })
       when not repair? or discarded == 0 do
    buffer
  end

  # repairs sequence number if there have been any packets discarded by any of previous elements
  defp repair_sequence_number(
         %Buffer{metadata: %{rtp: %{sequence_number: seq_num}} = metadata} = buffer,
         %State{discarded: discarded}
       ) do
    metadata =
      put_in(
        metadata,
        [:rtp, :sequence_number],
        rem(seq_num - discarded + @max_seq_num + 1, @max_seq_num + 1)
      )

    %Buffer{buffer | metadata: metadata}
  end
end
