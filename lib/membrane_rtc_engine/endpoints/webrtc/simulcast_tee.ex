defmodule Membrane.RTC.Engine.Endpoint.WebRTC.SimulcastTee do
  @moduledoc false
  use Membrane.Filter

  alias Membrane.RTC.Engine.Endpoint.WebRTC.EncodingTracker
  alias Membrane.RTC.Engine.Endpoint.WebRTC.Forwarder

  require Membrane.Logger
  require Membrane.TelemetryMetrics

  @supported_codecs [:H264, :VP8]

  def_options codec: [
                type: :atom,
                spec: [:H264 | :VP8],
                description: "Codec of track #{inspect(__MODULE__)} will forward."
              ],
              clock_rate: [
                type: :integer,
                spec: Membrane.RTP.clock_rate_t(),
                description: "Clock rate of track #{inspect(__MODULE__)} will forward."
              ],
              endpoint_id: [
                type: :binary,
                description:
                  "Identifier of WebRTC Endpoint forwarding tracks to #{inspect(__MODULE__)}"
              ]

  def_input_pad :input,
    availability: :on_request,
    mode: :pull,
    demand_mode: :auto,
    caps: Membrane.RTP

  def_output_pad :output,
    availability: :on_request,
    mode: :push,
    caps: Membrane.RTP

  @typedoc """
  Notifies that encoding for endpoint with id `endpoint_id` was switched to encoding `encoding`.
  """
  @type encoding_switched_notification_t() ::
          {:encoding_switched, endpoint_id :: any(), encoding :: String.t()}

  @impl true
  def handle_init(opts) do
    codec = opts.codec

    if codec not in @supported_codecs do
      raise("""
      #{inspect(__MODULE__)} does not support codec #{inspect(codec)}.
      Supported codecs: #{inspect(@supported_codecs)}
      """)
    end

    {:ok,
     %{
       codec: codec,
       clock_rate: opts.clock_rate,
       forwarders: %{},
       trackers: %{
         "l" => EncodingTracker.new("l"),
         "m" => EncodingTracker.new("m"),
         "h" => EncodingTracker.new("h")
       },
       update?: false,
       endpoint_id: opts.endpoint_id
     }}
  end

  @impl true
  def handle_pad_added(Pad.ref(:input, {_track_id, _rid}), _context, state) do
    {:ok, state}
  end

  @impl true
  def handle_pad_added(Pad.ref(:output, {:endpoint, endpoint_id}), _context, state) do
    state =
      put_in(state, [:forwarders, endpoint_id], Forwarder.new(state.codec, state.clock_rate))

    {:ok, state}
  end

  @impl true
  def handle_pad_added(pad, _context, _state) do
    raise("Pad #{inspect(pad)} not allowed for #{inspect(__MODULE__)}")
  end

  @impl true
  def handle_pad_removed(Pad.ref(:output, {:endpoint, endpoint_id}), _context, state) do
    {_forwarder, state} = pop_in(state, [:forwarders, endpoint_id])
    {:ok, state}
  end

  @impl true
  def handle_pad_removed(Pad.ref(:input, {_track_id, rid}), _context, state) do
    {_tracker, state} = pop_in(state, [:trackers, rid])
    {:ok, state}
  end

  @impl true
  def handle_prepared_to_playing(_context, state) do
    start_timer = [start_timer: {:check_encoding_statuses, 1_000_000_000}]
    {{:ok, start_timer}, state}
  end

  @impl true
  def handle_process(Pad.ref(:input, {track_id, encoding}), buffer, _context, state) do
    Membrane.TelemetryMetrics.execute(
      [:video_track, :packet_arrival],
      %{
        bitrate: bit_size(buffer.payload),
        keyframe_indicator: keyframe_indicator(buffer.payload, state.codec)
      },
      %{endpoint_id: state.endpoint_id, track_id: track_id, encoding: encoding}
    )

    Membrane.RTC.Utils.emit_telemetry_event_with_packet_mesaurments(
      buffer.payload,
      buffer.metadata.rtp.ssrc,
      state.codec
    )

    state = update_in(state, [:trackers, encoding], &EncodingTracker.increment_samples(&1))

    {actions, state} =
      Enum.flat_map_reduce(state.forwarders, state, fn
        {endpoint_id, forwarder}, state ->
          {forwarder, actions} = Forwarder.process(forwarder, buffer, encoding, endpoint_id)
          state = put_in(state, [:forwarders, endpoint_id], forwarder)
          {actions, state}
      end)

    {{:ok, actions}, state}
  end

  @impl true
  def handle_tick(:check_encoding_statuses, _ctx, state) do
    state =
      Enum.reduce(state.trackers, state, fn {rid, tracker}, state ->
        check_encoding_status(rid, tracker, state)
      end)

    {:ok, state}
  end

  defp check_encoding_status(rid, tracker, state) do
    case EncodingTracker.check_encoding_status(tracker) do
      {:ok, tracker} ->
        put_in(state, [:trackers, rid], tracker)

      {:status_changed, tracker, new_status} ->
        func =
          if new_status == :inactive,
            do: &Forwarder.encoding_inactive(&1, rid),
            else: &Forwarder.encoding_active(&1, rid)

        state =
          Enum.reduce(state.forwarders, state, fn {endpoint_id, forwarder}, state ->
            put_in(state, [:forwarders, endpoint_id], func.(forwarder))
          end)

        put_in(state, [:trackers, rid], tracker)
    end
  end

  @impl true
  def handle_other({:select_encoding, {endpoint_id, encoding}}, _ctx, state) do
    state =
      update_in(state, [:forwarders, endpoint_id], fn forwarder ->
        Forwarder.select_encoding(forwarder, encoding)
      end)

    {:ok, state}
  end

  @impl true
  def handle_end_of_stream(_pad, context, state) do
    all_end_of_streams? =
      context.pads
      |> Enum.filter(fn {_pad_name, pad_data} ->
        pad_data.direction == :input
      end)
      |> Enum.all?(fn {_pad_name, pad_data} ->
        pad_data.end_of_stream?
      end)

    if all_end_of_streams? do
      {{:ok, forward: :end_of_stream}, state}
    else
      {:ok, state}
    end
  end

  defp keyframe_indicator(payload, codec) do
    is_keyframe =
      case codec do
        :H264 -> Membrane.RTP.H264.Utils.is_keyframe(payload)
        :VP8 -> Membrane.RTP.VP8.Utils.is_keyframe(payload)
      end

    if is_keyframe, do: 1, else: 0
  end
end
