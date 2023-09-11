defmodule Membrane.RTP.TWCCSender do
  @moduledoc """
  The module defines an element responsible for tagging outgoing packets with transport-wide sequence numbers and
  estimating available bandwidth.
  """
  use Membrane.Filter

  require Bitwise

  alias __MODULE__.{CongestionControl, ReceiverRate}
  alias Membrane.RTCP.TransportFeedbackPacket.TWCC
  alias Membrane.RTP
  alias Membrane.RTP.Header
  alias Membrane.Time

  @seq_number_limit Bitwise.bsl(1, 16)

  def_input_pad :input, accepted_format: RTP, availability: :on_request, demand_mode: :auto
  def_output_pad :output, accepted_format: RTP, availability: :on_request, demand_mode: :auto

  @impl true
  def handle_init(_ctx, _options) do
    {[],
     %{
       seq_num: 0,
       seq_to_timestamp: %{},
       seq_to_size: %{},
       cc: %CongestionControl{},
       bandwidth_report_interval: Time.seconds(5),
       buffered_actions: %{}
     }}
  end

  @impl true
  def handle_pad_added(Pad.ref(direction, _ref) = pad, ctx, state) do
    {queued_actions, other_actions} = Map.pop(state.buffered_actions, pad, [])

    has_other_input_pads? =
      Enum.any?(ctx.pads, fn
        {ref, %{direction: :input}} -> ref != pad
        _else -> false
      end)

    # TWCC should be reset when adding the first input pad
    # This is especially helpful when we're deling with situation in which pads existed before
    # Otherwise, estimation will start from basically 0, and we want to start from the normal starting point
    state =
      if direction == :input and not has_other_input_pads? do
        %{state | cc: %CongestionControl{}}
      else
        state
      end

    {Enum.to_list(queued_actions), %{state | buffered_actions: other_actions}}
  end

  @impl true
  def handle_stream_format(Pad.ref(:input, id), stream_format, ctx, state) do
    out_pad = Pad.ref(:output, id)
    [stream_format: {out_pad, stream_format}] |> send_when_pad_connected(out_pad, ctx, state)
  end

  @impl true
  def handle_playing(_ctx, state) do
    {[start_timer: {:bandwidth_report_timer, state.bandwidth_report_interval}], state}
  end

  @impl true
  def handle_tick(
        :bandwidth_report_timer,
        _ctx,
        %{cc: %CongestionControl{r_hat: %ReceiverRate{value: nil}}} = state
      ) do
    # wait until the first r_hat is calculated
    {[], state}
  end

  @impl true
  def handle_tick(:bandwidth_report_timer, _ctx, %{cc: cc} = state) do
    {[notify_parent: {:bandwidth_estimation, min(cc.a_hat, cc.as_hat)}], state}
  end

  @impl true
  def handle_event(Pad.ref(direction, id), event, ctx, state) do
    opposite_direction = if direction == :input, do: :output, else: :input
    out_pad = Pad.ref(opposite_direction, id)
    [event: {out_pad, event}] |> send_when_pad_connected(out_pad, ctx, state)
  end

  @impl true
  def handle_parent_notification({:twcc_feedback, feedback}, _ctx, state) do
    %TWCC{
      # TODO: consider what to do when we lose some feedback
      feedback_packet_count: _feedback_packet_count,
      reference_time: reference_time,
      base_seq_num: base_seq_num,
      packet_status_count: packet_count,
      receive_deltas: receive_deltas
    } = feedback

    max_seq_num = base_seq_num + packet_count - 1

    rtt =
      Time.vm_time() -
        Map.fetch!(state.seq_to_timestamp, rem(max_seq_num, @seq_number_limit))

    sequence_numbers = Enum.map(base_seq_num..max_seq_num, &rem(&1, @seq_number_limit))

    send_timestamps = Enum.map(sequence_numbers, &Map.fetch!(state.seq_to_timestamp, &1))

    timestamp_before_base = Map.get(state.seq_to_timestamp, base_seq_num - 1, hd(send_timestamps))

    send_deltas =
      sequence_numbers
      |> Enum.map_reduce(timestamp_before_base, fn seq_num, previous_timestamp ->
        timestamp = Map.fetch!(state.seq_to_timestamp, seq_num)
        {timestamp - previous_timestamp, timestamp}
      end)
      |> elem(0)

    packet_sizes = Enum.map(sequence_numbers, &Map.fetch!(state.seq_to_size, &1))

    cc =
      CongestionControl.update(
        state.cc,
        reference_time,
        receive_deltas,
        send_deltas,
        packet_sizes,
        rtt
      )

    {[], %{state | cc: cc}}
  end

  @impl true
  def handle_process(Pad.ref(:input, id), buffer, ctx, state) do
    {seq_num, state} = Map.get_and_update!(state, :seq_num, &{&1, rem(&1 + 1, @seq_number_limit)})

    buffer =
      Header.Extension.put(buffer, %Header.Extension{identifier: :twcc, data: <<seq_num::16>>})

    # TODO take into account header size
    # we are not taking into account header
    # size here which doesn't seem to be
    # fully correct
    padding_size = Map.get(buffer.metadata.rtp, :padding_size, 0)

    unless padding_size in 0..255, do: raise("padding_size has to be in 0..255")

    overall_payload_size = bit_size(buffer.payload) + padding_size * 8

    state =
      state
      |> put_in([:seq_to_timestamp, seq_num], Time.vm_time())
      |> put_in([:seq_to_size, seq_num], overall_payload_size)

    out_pad = Pad.ref(:output, id)
    [buffer: {out_pad, buffer}] |> send_when_pad_connected(out_pad, ctx, state)
  end

  @impl true
  def handle_end_of_stream(Pad.ref(:input, id), ctx, state) do
    out_pad = Pad.ref(:output, id)
    [end_of_stream: out_pad] |> send_when_pad_connected(out_pad, ctx, state)
  end

  defp send_when_pad_connected(actions, pad, ctx, state) do
    if Map.has_key?(ctx.pads, pad) do
      {actions, state}
    else
      state =
        update_in(state, [:buffered_actions, pad], &Qex.join(&1 || Qex.new(), Qex.new(actions)))

      {[], state}
    end
  end
end
