defmodule Membrane.RTC.Engine.Endpoint.File do
  @moduledoc """
  An Endpoint responsible for publishing data from a file (Currently supports only OPUS and H264).
  It starts publishing data on receiving `:start` message.
  After publishing track it sends to engine parent notification `:tracks_added`.
  """

  use Membrane.Bin

  require Membrane.Logger

  alias Membrane.ChildrenSpec
  alias Membrane.RTC.Engine
  alias Membrane.RTC.Engine.Support.StaticTrackSender
  alias Membrane.RTC.Engine.Track
  alias Membrane.RTP
  alias Membrane.RTP.PayloaderBin

  @type encoding_t() :: String.t()

  def_options rtc_engine: [
                spec: pid(),
                description: "Pid of parent Engine"
              ],
              file_path: [
                spec: Path.t(),
                description: "Path to track file"
              ],
              track: [
                spec: Engine.Track.t(),
                description: "Track to publish"
              ],
              ssrc: [
                spec: RTP.ssrc_t(),
                default: 1,
                description: "SSRC of RTP packets"
              ],
              payload_type: [
                spec: RTP.payload_type_t(),
                defalt: 127,
                description: "Payload type of RTP packets"
              ],
              toilet_capacity: [
                spec: integer(),
                default: 1_000,
                description:
                  "Size of toilet between payloader and realtimer and between realtimer and track_sender"
              ],
              after_source_transformation: [
                spec: (ChildrenSpec.builder(), Track.t() -> ChildrenSpec.builder()),
                default: &__MODULE__.identitiy_transformation/1,
                description:
                  "Additional pipeline transformation after `file_source` the output stream must be encoded in OPUS or H264"
              ]

  def_output_pad :output,
    demand_unit: :buffers,
    accepted_format: _any,
    availability: :on_request

  @spec start_sending(pid(), any()) :: :ok
  def start_sending(engine, endpoint_id) do
    Engine.message_endpoint(engine, endpoint_id, :start)
  end

  @impl true
  def handle_init(_ctx, opts) do
    unless Enum.any?([:H264, :OPUS], &(&1 == opts.track.encoding)) do
      raise "Unsupported track codec: #{inspect(opts.track.encoding)}. The only supported codecs are :H264 and :OPUS."
    end

    {[notify_parent: {:ready, nil}], Map.from_struct(opts)}
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
    payloader = Membrane.RTP.PayloadFormat.get(state.track.encoding).payloader

    payloader_bin = %PayloaderBin{
      payloader: payloader,
      ssrc: state.ssrc,
      payload_type: state.payload_type,
      clock_rate: state.track.clock_rate
    }

    spec = [
      child(:source, %Membrane.File.Source{location: state.file_path})
      |> then(&state.after_source_transformation.(&1))
      |> child(:payloader, payloader_bin)
      |> via_in(:input, toilet_capacity: state.toilet_capacity)
      |> child(:realtimer, Membrane.Realtimer)
      |> via_in(:input, toilet_capacity: state.toilet_capacity)
      |> child(:track_sender, %StaticTrackSender{
        track: state.track,
        is_keyframe: fn buffer, track ->
          case track.encoding do
            :OPUS -> true
            :H264 -> Membrane.RTP.H264.Utils.is_keyframe(buffer.payload)
            :VP8 -> Membrane.RTP.VP8.Utils.is_keyframe(buffer.payload)
          end
        end
      })
      |> bin_output(pad)
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

  @spec identitiy_transformation(ChildrenSpec.builder()) :: ChildrenSpec.builder()
  def identitiy_transformation(link_builder), do: link_builder

  @spec transform_aac_to_opus(ChildrenSpec.builder()) :: ChildrenSpec.builder()
  def transform_aac_to_opus(link_builder) do
    link_builder
    |> child(:decoder, Membrane.AAC.FDK.Decoder)
    |> child(:encoder, %Membrane.Opus.Encoder{
      input_stream_format: %Membrane.RawAudio{
        channels: 1,
        sample_rate: 48_000,
        sample_format: :s16le
      }
    })
    |> child(:parser, %Membrane.Opus.Parser{})
  end
end
