defmodule Membrane.RTP.SilenceDiscarder do
  @moduledoc """
  Element responsible for dropping silent audio packets.

  For a packet to be discarded it needs to contain a `RTP.Header.Extension` struct with identifier equal to `vad_id` in
  its extensions list. The header extension will contain information about audio level (VAD extension is required).
  The element will only drop packets whose audio level is above given silence threshold (muted audio is of value 127).

  `#{__MODULE__}` will drop as many silent packets as possible and on reaching dropping limit it will send the current buffer,
  reset dropped packets counter and emit `Membrane.RTP.DroppedPacketEvent` with a number of packets that have been dropped until that point.
  The event gets sent on both reaching dropping limit and when a non-silent packet arrives.
  """
  use Membrane.Filter

  alias Membrane.RTP.{Header, PacketsDiscardedEvent}

  def_input_pad :input, accepted_format: _any, demand_mode: :auto
  def_output_pad :output, accepted_format: _any, demand_mode: :auto

  def_options max_consecutive_drops: [
                spec: non_neg_integer() | :infinity,
                default: 1000,
                description: """
                A number indicating how many consecutive silent packets can be dropped before
                a single packet will be passed and dropped packet event will we emitted.

                Passing a single packets once in a while is necessary for element such as jitter buffer or encryptor as they can update their ROCs
                based on sequence numbers and when we drop to many packets we may roll it over.
                """
              ],
              silence_threshold: [
                spec: 1..127,
                default: 127,
                description: """
                Audio level threshold that will be compared against incoming packets. Packet will be dropped if its audio level
                is above or equal to the given threshold.
                """
              ],
              vad_id: [
                spec: 1..14,
                default: 1,
                description: """
                ID of a VAD extension.
                """
              ]

  @impl true
  def handle_init(_ctx, opts) do
    {[], Map.from_struct(opts) |> Map.put(:dropped, 0)}
  end

  @impl true
  def handle_event(pad, other, ctx, state), do: super(pad, other, ctx, state)

  @impl true
  def handle_process(
        :input,
        buffer,
        _ctx,
        %{dropped: dropped, max_consecutive_drops: max_drops} = state
      )
      when dropped == max_drops do
    stop_dropping(buffer, state)
  end

  @impl true
  def handle_process(:input, buffer, _ctx, state) do
    buffer
    |> Header.Extension.find(state.vad_id)
    |> handle_vad(buffer, state)
  end

  defp handle_vad(nil, buffer, state), do: {[buffer: {:output, buffer}], state}

  defp handle_vad(vad, buffer, state) do
    %{dropped: dropped, silence_threshold: silence_threshold} = state
    <<_v::1, audio_level::7>> = vad.data

    cond do
      audio_level >= silence_threshold ->
        {[], %{state | dropped: dropped + 1}}

      dropped > 0 ->
        stop_dropping(buffer, state)

      true ->
        {[buffer: {:output, buffer}], state}
    end
  end

  defp stop_dropping(buffer, state) do
    {[
       event: {:output, %PacketsDiscardedEvent{discarded: state.dropped}},
       buffer: {:output, buffer}
     ], %{state | dropped: 0}}
  end
end
