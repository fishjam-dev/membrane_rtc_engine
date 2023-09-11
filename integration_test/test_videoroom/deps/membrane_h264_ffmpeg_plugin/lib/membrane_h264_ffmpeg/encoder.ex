defmodule Membrane.H264.FFmpeg.Encoder do
  @moduledoc """
  Membrane element that encodes raw video frames to H264 format.

  The element expects each frame to be received in a separate buffer, so the parser
  (`Membrane.Element.RawVideo.Parser`) may be required in a pipeline before
  the encoder (e.g. when input is read from `Membrane.File.Source`).

  Additionally, the encoder has to receive proper stream_format with picture format and dimensions
  before any encoding takes place.

  Please check `t:t/0` for available options.
  """
  use Membrane.Filter
  alias __MODULE__.Native
  alias Membrane.Buffer
  alias Membrane.H264
  alias Membrane.H264.FFmpeg.Common
  alias Membrane.RawVideo

  def_input_pad :input,
    demand_mode: :auto,
    demand_unit: :buffers,
    accepted_format: %RawVideo{pixel_format: format, aligned: true} when format in [:I420, :I422]

  def_output_pad :output,
    demand_mode: :auto,
    accepted_format: %H264{alignment: :au}

  @default_crf 23

  @type preset() ::
          :ultrafast
          | :superfast
          | :veryfast
          | :faster
          | :fast
          | :medium
          | :slow
          | :slower
          | :veryslow
          | :placebo

  @type tune() ::
          :film
          | :animation
          | :grain
          | :stillimage
          | :fastdecode
          | :zerolatency

  def_options crf: [
                description: """
                Constant rate factor that affects the quality of output stream.
                Value of 0 is lossless compression while 51 (for 8-bit samples)
                or 63 (10-bit) offers the worst quality.
                The range is exponential, so increasing the CRF value +6 results
                in roughly half the bitrate / file size, while -6 leads
                to roughly twice the bitrate.
                """,
                spec: 0..63,
                default: @default_crf
              ],
              preset: [
                description: """
                Collection of predefined options providing certain encoding.
                The slower the preset chosen, the higher compression for the
                same quality can be achieved.
                """,
                spec: preset(),
                default: :medium
              ],
              profile: [
                description: """
                Sets a limit on the features that the encoder will use to the ones supported in a provided H264 profile.
                Said features will have to be supported by the decoder in order to decode the resulting video.
                It may override other, more specific options affecting compression (e.g setting `max_b_frames` to 2
                while profile is set to `:baseline` will have no effect and no B-frames will be present).
                """,
                spec: H264.profile_t() | nil,
                default: nil
              ],
              tune: [
                description: """
                Optionally tune the encoder settings for a particular type of source or situation.
                See [`x264` encoder's man page](https://manpages.ubuntu.com/manpages/trusty/man1/x264.1.html) for more info.
                Available options are:
                - `:film` - use for high quality movie content; lowers deblocking
                - `:animation` - good for cartoons; uses higher deblocking and more reference frames
                - `:grain` - preserves the grain structure in old, grainy film material
                - `:stillimage` - good for slideshow-like content
                - `:fastdecode` - allows faster decoding by disabling certain filters
                - `:zerolatency` - good for fast encoding and low-latency streaming
                """,
                spec: tune() | nil,
                default: nil
              ],
              use_shm?: [
                spec: boolean(),
                desciption:
                  "If true, native encoder will use shared memory (via `t:Shmex.t/0`) for storing frames",
                default: false
              ],
              max_b_frames: [
                spec: non_neg_integer() | nil,
                description:
                  "Maximum number of B-frames between non-B-frames. Set to 0 to encode video without b-frames",
                default: nil
              ],
              gop_size: [
                spec: non_neg_integer() | nil,
                description: "Number of frames in a group of pictures.",
                default: nil
              ]

  @impl true
  def handle_init(_ctx, opts) do
    state =
      opts
      |> Map.put(:encoder_ref, nil)

    {[], state}
  end

  @impl true
  def handle_process(:input, buffer, _ctx, state) do
    %{encoder_ref: encoder_ref, use_shm?: use_shm?} = state
    pts = Common.to_h264_time_base_truncated(buffer.pts)

    case Native.encode(
           buffer.payload,
           pts,
           use_shm?,
           encoder_ref
         ) do
      {:ok, dts_list, pts_list, frames} ->
        bufs = wrap_frames(dts_list, pts_list, frames)

        {bufs, state}

      {:error, reason} ->
        raise "Native encoder failed to encode the payload: #{inspect(reason)}"
    end
  end

  @impl true
  def handle_stream_format(:input, stream_format, _ctx, state) do
    {framerate_num, framerate_denom} = stream_format.framerate

    with buffers <- flush_encoder_if_exists(state),
         {:ok, new_encoder_ref} <-
           Native.create(
             stream_format.width,
             stream_format.height,
             stream_format.pixel_format,
             state.preset,
             state.tune,
             state.profile,
             state.max_b_frames || -1,
             state.gop_size || -1,
             framerate_num,
             framerate_denom,
             state.crf
           ) do
      stream_format = create_new_stream_format(stream_format, state)
      actions = buffers ++ [stream_format: stream_format]
      {actions, %{state | encoder_ref: new_encoder_ref}}
    else
      {:error, reason} -> raise "Failed to create native encoder: #{inspect(reason)}"
    end
  end

  @impl true
  def handle_end_of_stream(:input, _ctx, state) do
    buffers = flush_encoder_if_exists(state)
    actions = buffers ++ [end_of_stream: :output]
    {actions, state}
  end

  defp flush_encoder_if_exists(%{encoder_ref: nil}), do: []

  defp flush_encoder_if_exists(%{encoder_ref: encoder_ref, use_shm?: use_shm?}) do
    with {:ok, dts_list, pts_list, frames} <- Native.flush(use_shm?, encoder_ref) do
      wrap_frames(dts_list, pts_list, frames)
    else
      {:error, reason} -> raise "Native encoder failed to flush: #{inspect(reason)}"
    end
  end

  defp wrap_frames([], [], []), do: []

  defp wrap_frames(dts_list, pts_list, frames) do
    Enum.zip([dts_list, pts_list, frames])
    |> Enum.map(fn {dts, pts, frame} ->
      %Buffer{
        pts: Common.to_membrane_time_base_truncated(pts),
        dts: Common.to_membrane_time_base_truncated(dts),
        payload: frame
      }
    end)
    |> then(&[buffer: {:output, &1}])
  end

  defp create_new_stream_format(stream_format, state) do
    {:output,
     %H264{
       alignment: :au,
       framerate: stream_format.framerate,
       height: stream_format.height,
       width: stream_format.width,
       profile: state.profile
     }}
  end
end
