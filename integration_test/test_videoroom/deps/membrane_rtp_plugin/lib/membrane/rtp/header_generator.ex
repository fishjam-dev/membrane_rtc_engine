defmodule Membrane.RTP.HeaderGenerator do
  @moduledoc """
  Given following RTP payloads and their minimal metadata, creates their proper header information,
  incrementing timestamps and sequence numbers for each packet. Header information then is put
  inside buffer's metadata under `:rtp` key.

  Accepts the following metadata under `:rtp` key: `:marker`, `:csrcs`, `:extensions`.
  See `Membrane.RTP.Header` for their meaning and specifications.
  """
  use Membrane.Filter

  require Bitwise
  alias Membrane.RTP

  @max_seq_num Bitwise.bsl(1, 16) - 1
  @max_timestamp Bitwise.bsl(1, 32) - 1

  def_input_pad :input, accepted_format: RTP, demand_mode: :auto

  def_output_pad :output, accepted_format: RTP, demand_mode: :auto

  def_options ssrc: [spec: RTP.ssrc_t()],
              payload_type: [spec: RTP.payload_type_t()],
              clock_rate: [spec: RTP.clock_rate_t()]

  defmodule State do
    @moduledoc false
    use Bunch.Access

    defstruct [
      :ssrc,
      :payload_type,
      :clock_rate,
      sequence_number: 0,
      init_timestamp: 0
    ]

    @type t :: %__MODULE__{
            ssrc: RTP.ssrc_t(),
            payload_type: RTP.payload_type_t(),
            clock_rate: RTP.clock_rate_t(),
            sequence_number: non_neg_integer(),
            init_timestamp: non_neg_integer()
          }
  end

  @impl true
  def handle_init(_ctx, options) do
    state =
      options
      |> Map.from_struct()
      |> Map.merge(%{
        sequence_number: Enum.random(0..@max_seq_num),
        init_timestamp: Enum.random(0..@max_timestamp)
      })

    {[], struct!(State, state)}
  end

  @impl true
  def handle_stream_format(:input, _stream_format, _ctx, state) do
    {[stream_format: {:output, %RTP{}}], state}
  end

  @impl true
  def handle_process(:input, buffer, _ctx, state) do
    {rtp_metadata, metadata} = Map.pop(buffer.metadata, :rtp, %{})

    rtp_offset =
      buffer.pts
      |> Ratio.mult(state.clock_rate)
      |> Membrane.Time.round_to_seconds()

    rtp_timestamp = rem(state.init_timestamp + rtp_offset, @max_timestamp + 1)

    state = Map.update!(state, :sequence_number, &rem(&1 + 1, @max_seq_num + 1))

    header = %{
      ssrc: state.ssrc,
      marker: Map.get(rtp_metadata, :marker, false),
      payload_type: state.payload_type,
      timestamp: rtp_timestamp,
      sequence_number: state.sequence_number,
      csrcs: Map.get(rtp_metadata, :csrcs, []),
      extensions: Map.get(rtp_metadata, :extensions, [])
    }

    buffer = %Membrane.Buffer{
      buffer
      | metadata: Map.put(metadata, :rtp, header),
        payload: buffer.payload
    }

    {[buffer: {:output, buffer}], state}
  end
end
