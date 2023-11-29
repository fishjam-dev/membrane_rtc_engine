defmodule Membrane.RTC.Engine.Endpoint.File do
  @moduledoc """
  An Endpoint responsible for publishing data from a file.
  By default only supports OPUS inside Ogg container and raw H264 files.
  By providing proper value in option `after_source_transformation` you can read other formats, but the output from `after_source_transformation` have to be encoded in OPUS or H264.
  It starts publishing data after calling function `start_sending` with proper arguments.
  After publishing track it sends to engine parent notification `:tracks_added`.
  After sending all data from file it sends to engine parent notification `:finished`.
  """

  use Membrane.Bin

  require Membrane.Logger

  alias Membrane.ChildrenSpec
  alias Membrane.RTC.Engine
  alias Membrane.RTC.Engine.Endpoint.File.TrackConfig
  alias Membrane.RTC.Engine.Support.StaticTrackSender
  alias Membrane.RTC.Engine.Track
  alias Membrane.RTP
  alias Membrane.RTP.PayloaderBin

  @type encoding_t() :: String.t()

  @toilet_capacity 1_000

  def_options rtc_engine: [
                spec: pid(),
                description: "Pid of parent Engine"
              ],
              file_path: [
                spec: Path.t(),
                description: "Path to track file"
              ],
              track_config: [
                spec: TrackConfig.t(),
                description: "Configuration of the track being published"
              ],
              ssrc: [
                spec: RTP.ssrc_t(),
                description: "SSRC of RTP packets",
                default: nil
              ],
              payload_type: [
                spec: RTP.payload_type_t(),
                description: "Payload type of RTP packets"
              ],
              after_source_transformation: [
                spec: (ChildrenSpec.builder() -> ChildrenSpec.builder()),
                default: &Function.identity/1,
                description: """
                Additional pipeline transformation after `file_source`.
                The output stream must be encoded in OPUS or H264.

                Example usage:
                * Reading ACC file: `fn link_builder ->  link_builder
                |> child(:decoder, Membrane.AAC.FDK.Decoder)
                |> child(:encoder, %Membrane.Opus.Encoder{input_stream_format:
                %Membrane.RawAudio{ channels: 1, sample_rate: 48_000, sample_format: :s16le }}) end`
                """
              ]

  def_output_pad :output,
    demand_unit: :buffers,
    accepted_format: Membrane.RTP,
    availability: :on_request

  @spec start_sending(pid(), any()) :: :ok
  def start_sending(engine, endpoint_id) do
    Engine.message_endpoint(engine, endpoint_id, :start)
  end

  @impl true
  def handle_init(ctx, opts) do
    unless Enum.any?([:H264, :OPUS], &(&1 == opts.track_config.encoding)) do
      raise "Unsupported track codec: #{inspect(opts.track_config.encoding)}. The only supported codecs are :H264 and :OPUS."
    end

    {:endpoint, endpoint_id} = ctx.name

    track =
      Track.new(
        opts.track_config.type,
        opts.track_config.stream_id || Track.stream_id(),
        endpoint_id,
        opts.track_config.encoding,
        opts.track_config.clock_rate,
        opts.track_config.fmtp,
        opts.track_config.opts
      )

    state =
      Map.from_struct(opts)
      |> Map.drop([:track_config])
      |> Map.merge(%{
        track: track,
        ssrc: opts.ssrc || new_ssrc(),
        demuxer_to_pad: %{}
      })

    {[notify_parent: {:ready, nil}], state}
  end

  @impl true
  def handle_playing(_ctx, state) do
    {[
       notify_parent: {:publish, {:new_tracks, [state.track]}},
       notify_parent: {:forward_to_parent, :tracks_added}
     ], state}
  end

  @impl true
  def handle_pad_added(Pad.ref(:output, {_track_id, _rid}) = pad, _ctx, state) do
    if state.track.encoding == :OPUS and
         state.after_source_transformation == (&Function.identity/1) do
      build_pipeline_ogg_demuxer(state, pad)
    else
      build_full_pipeline(state, pad)
    end
  end

  @impl true
  def handle_child_notification(
        {:new_track, {track_id, :opus}},
        {:ogg_demuxer, demuxer_id} = child_name,
        _ctx,
        state
      ) do
    {output_pad, state} = pop_in(state.demuxer_to_pad[demuxer_id])

    spec = [
      get_child(child_name)
      |> via_out(Pad.ref(:output, track_id))
      |> child(:parser, %Membrane.Opus.Parser{})
      |> then(get_rest_of_pipeline(state, output_pad))
    ]

    {[spec: spec], state}
  end

  @impl true
  def handle_element_end_of_stream(:track_sender, _pad, _ctx, state) do
    {[notify_parent: {:forward_to_parent, :finished}], state}
  end

  @impl true
  def handle_element_end_of_stream(_other, _pad, _ctx, state) do
    {[], state}
  end

  @impl true
  def handle_parent_notification({:ready, _other_endpoints}, _ctx, state) do
    {[], state}
  end

  @impl true
  def handle_parent_notification({:new_endpoint, _endpoint}, _ctx, state) do
    {[], state}
  end

  @impl true
  def handle_parent_notification({:endpoint_removed, _endpoint_id}, _ctx, state) do
    {[], state}
  end

  @impl true
  def handle_parent_notification({:new_tracks, _list}, _ctx, state) do
    {[], state}
  end

  @impl true
  def handle_parent_notification({:remove_tracks, _list}, _ctx, state) do
    {[], state}
  end

  @impl true
  def handle_parent_notification(:start, _ctx, state) do
    track_ready = {:track_ready, state.track.id, :high, state.track.encoding}
    {[notify_parent: track_ready], state}
  end

  defp get_parser(%Track{encoding: :OPUS}) do
    fn link_builder ->
      child(link_builder, :parser, %Membrane.Opus.Parser{})
    end
  end

  defp build_pipeline_ogg_demuxer(state, pad) do
    demuxer_id = UUID.uuid4()

    demuxer = fn link_builder ->
      child(link_builder, {:ogg_demuxer, demuxer_id}, Membrane.Ogg.Demuxer)
    end

    state = put_in(state.demuxer_to_pad[demuxer_id], pad)

    spec = [
      child(:source, %Membrane.File.Source{location: state.file_path})
      |> then(demuxer)
    ]

    {[spec: spec], state}
  end

  defp build_full_pipeline(state, pad) do
    parser = get_parser(state.track)

    spec = [
      child(:source, %Membrane.File.Source{location: state.file_path})
      |> then(&state.after_source_transformation.(&1))
      |> then(parser)
      |> then(get_rest_of_pipeline(state, pad))
    ]

    {[spec: spec], state}
  end

  defp get_rest_of_pipeline(state, pad) do
    payloader = Membrane.RTP.PayloadFormat.get(state.track.encoding).payloader

    payloader_bin = %PayloaderBin{
      payloader: payloader,
      ssrc: state.ssrc,
      payload_type: state.payload_type,
      clock_rate: state.track.clock_rate
    }

    fn link_builder ->
      link_builder
      |> child(:payloader, payloader_bin)
      |> via_in(:input, toilet_capacity: @toilet_capacity)
      |> child(:realtimer, Membrane.Realtimer)
      |> via_in(:input, toilet_capacity: @toilet_capacity)
      |> child(:track_sender, %StaticTrackSender{
        track: state.track,
        is_keyframe: fn buffer, track ->
          case track.encoding do
            :OPUS -> true
            :H264 -> Membrane.RTP.H264.Utils.is_keyframe(buffer.payload)
          end
        end
      })
      |> bin_output(pad)
    end
  end

  defp get_parser(%Track{encoding: :H264, framerate: framerate}) do
    fn link_builder ->
      child(link_builder, :parser, %Membrane.H264.Parser{
        generate_best_effort_timestamps: %{
          framerate: framerate
        },
        output_alignment: :nalu
      })
    end
  end

  defp new_ssrc() do
    :crypto.strong_rand_bytes(4) |> :binary.decode_unsigned()
  end
end
