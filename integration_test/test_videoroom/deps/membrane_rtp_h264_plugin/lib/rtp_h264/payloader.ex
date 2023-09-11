defmodule Membrane.RTP.H264.Payloader do
  @moduledoc """
  Payloads H264 NAL Units into H264 RTP payloads.

  Based on [RFC 6184](https://tools.ietf.org/html/rfc6184)

  Supported types: Single NALU, FU-A, STAP-A.
  """

  use Bunch
  use Membrane.Filter

  alias Membrane.Buffer
  alias Membrane.H264
  alias Membrane.RTP
  alias Membrane.RTP.H264.{FU, StapA}

  def_options max_payload_size: [
                spec: non_neg_integer(),
                default: 1400,
                description: """
                Maximal size of outputted payloads in bytes. Doesn't work in
                the `single_nalu` mode. The resulting RTP packet will also contain
                RTP header (12B) and potentially RTP extensions. For most
                applications, everything should fit in standard MTU size (1500B)
                after adding L3 and L2 protocols' overhead.
                """
              ],
              mode: [
                spec: :single_nalu | :non_interleaved,
                default: :non_interleaved,
                description: """
                In `:single_nalu` mode, payloader puts exactly one NAL unit
                into each payload, altering only RTP metadata. `:non_interleaved`
                mode handles also FU-A and STAP-A packetization. See
                [RFC 6184](https://tools.ietf.org/html/rfc6184) for details.
                """
              ]

  def_input_pad :input,
    accepted_format: %H264{alignment: :nal},
    demand_mode: :auto

  def_output_pad :output, accepted_format: RTP, demand_mode: :auto

  defmodule State do
    @moduledoc false
    defstruct [
      :max_payload_size,
      :mode,
      stap_acc: %{
        payloads: [],
        # header size
        byte_size: 1,
        pts: 0,
        dts: 0,
        metadata: nil,
        nri: 0,
        reserved: 0
      }
    ]
  end

  @impl true
  def handle_init(_ctx, opts) do
    {[], Map.merge(%State{}, Map.from_struct(opts))}
  end

  @impl true
  def handle_playing(_ctx, state) do
    {[stream_format: {:output, %RTP{}}], state}
  end

  @impl true
  def handle_stream_format(:input, _stream_format, _context, state) do
    {[], state}
  end

  @impl true
  def handle_process(:input, %Buffer{} = buffer, _ctx, state) do
    buffer = Map.update!(buffer, :payload, &delete_prefix/1)

    {buffers, state} =
      withl mode: :non_interleaved <- state.mode,
            stap_a: {:deny, stap_acc_bufs, state} <- try_stap_a(buffer, state),
            single_nalu: :deny <- try_single_nalu(buffer, state) do
        {stap_acc_bufs ++ use_fu_a(buffer, state), state}
      else
        mode: :single_nalu -> {use_single_nalu(buffer), state}
        stap_a: {:accept, buffers, state} -> {buffers, state}
        single_nalu: {:accept, buffer} -> {stap_acc_bufs ++ [buffer], state}
      end

    {[buffer: {:output, buffers}], state}
  end

  @impl true
  def handle_end_of_stream(:input, _ctx, state) do
    {buffers, state} = flush_stap_acc(state)
    {[buffer: {:output, buffers}, end_of_stream: :output], state}
  end

  defp delete_prefix(<<0, 0, 0, 1, nal::binary>>), do: nal
  defp delete_prefix(<<0, 0, 1, nal::binary>>), do: nal

  defp try_stap_a(buffer, state) do
    with {:deny, acc_buffers, state} <- do_try_stap_a(buffer, state) do
      # try again, after potential accumulator flush
      {result, [], state} = do_try_stap_a(buffer, state)
      {result, acc_buffers, state}
    end
  end

  defp do_try_stap_a(buffer, state) do
    %{stap_acc: stap_acc} = state
    size = stap_acc.byte_size + StapA.aggregation_unit_size(buffer.payload)
    metadata_match? = !stap_acc.metadata || stap_acc.pts == buffer.pts

    if metadata_match? and size <= state.max_payload_size do
      <<r::1, nri::2, _type::5, _rest::binary>> = buffer.payload

      stap_acc = %{
        stap_acc
        | payloads: [buffer.payload | stap_acc.payloads],
          byte_size: size,
          metadata: stap_acc.metadata || buffer.metadata,
          pts: buffer.pts,
          dts: buffer.dts,
          reserved: stap_acc.reserved * r,
          nri: min(stap_acc.nri, nri)
      }

      {:accept, [], %{state | stap_acc: stap_acc}}
    else
      {buffers, state} = flush_stap_acc(state)
      {:deny, buffers, state}
    end
  end

  defp flush_stap_acc(%{stap_acc: stap_acc} = state) do
    buffers =
      case stap_acc.payloads do
        [] ->
          []

        [payload] ->
          # use single nalu
          [
            %Buffer{
              payload: payload,
              metadata: stap_acc.metadata,
              pts: stap_acc.pts,
              dts: stap_acc.dts
            }
            |> set_marker()
          ]

        payloads ->
          payload = StapA.serialize(payloads, stap_acc.reserved, stap_acc.nri)

          [
            %Buffer{
              payload: payload,
              metadata: stap_acc.metadata,
              pts: stap_acc.pts,
              dts: stap_acc.dts
            }
            |> set_marker()
          ]
      end

    {buffers, %{state | stap_acc: %State{}.stap_acc}}
  end

  defp try_single_nalu(buffer, state) do
    if byte_size(buffer.payload) <= state.max_payload_size do
      {:accept, use_single_nalu(buffer)}
    else
      :deny
    end
  end

  defp use_single_nalu(buffer) do
    set_marker(buffer)
  end

  defp use_fu_a(buffer, state) do
    buffer.payload
    |> FU.serialize(state.max_payload_size)
    |> Enum.map(&%Buffer{buffer | payload: &1})
    |> Enum.map(&clear_marker/1)
    |> List.update_at(-1, &set_marker/1)
  end

  defp set_marker(buffer) do
    marker = Map.get(buffer.metadata.h264, :end_access_unit, false)
    Bunch.Struct.put_in(buffer, [:metadata, :rtp], %{marker: marker})
  end

  defp clear_marker(buffer) do
    Bunch.Struct.put_in(buffer, [:metadata, :rtp], %{marker: false})
  end
end
