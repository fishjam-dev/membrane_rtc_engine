defmodule Membrane.RTC.Engine.Support.FileEndpoint do
  @moduledoc false

  # Endpoint that publishes data from a file.
  # Starts publishing data on receiving `:start` message.

  use Membrane.Bin

  require Membrane.Logger

  alias Membrane.RTC.Engine
  alias Membrane.RTC.Engine.Support.StaticTrackSender
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
              framerate: [
                spec: {frames :: pos_integer, seconds :: pos_integer} | nil,
                default: nil,
                description: "Framerate in case of video track"
              ],
              ssrc: [
                spec: RTP.ssrc_t(),
                description: "SSRC of RTP packets"
              ],
              payload_type: [
                spec: RTP.payload_type_t(),
                description: "Payload type of RTP packets"
              ]

  def_output_pad :output,
    demand_unit: :buffers,
    accepted_format: _any,
    availability: :on_request

  @impl true
  def handle_init(_ctx, opts) do
    if opts.track.encoding != :H264 do
      raise "Unsupported track codec: #{inspect(opts.track.encoding)}. The only supported codec is :H264."
    end

    {[], Map.from_struct(opts)}
  end

  @impl true
  def handle_playing(_ctx, state) do
    {[notify_parent: {:publish, {:new_tracks, [state.track]}}], state}
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

    spec =
      child(:source, %Membrane.File.Source{location: state.file_path})
      |> child(:parser, %Membrane.H264.FFmpeg.Parser{framerate: state.framerate, alignment: :nal})
      |> child(:payloader, payloader_bin)
      |> child(:track_sender, %StaticTrackSender{track: state.track})
      |> bin_output(pad)

    {[spec: spec], state}
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
end
