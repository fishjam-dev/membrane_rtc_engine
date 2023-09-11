defmodule Membrane.H264.FFmpeg.Decoder do
  @moduledoc """
  Membrane element that decodes video in H264 format. It is backed by decoder from FFmpeg.

  The element expects the data for each frame (Access Unit) to be received in a separate buffer,
  so the parser (`Membrane.H264.FFmpeg.Parser`) may be required in a pipeline before
  decoder (e.g. when input is read from `Membrane.File.Source`).
  """
  use Membrane.Filter

  require Membrane.Logger

  alias __MODULE__.Native
  alias Membrane.Buffer
  alias Membrane.H264
  alias Membrane.H264.FFmpeg.Common
  alias Membrane.RawVideo

  def_options use_shm?: [
                spec: boolean(),
                desciption:
                  "If true, native decoder will use shared memory (via `t:Shmex.t/0`) for storing frames",
                default: false
              ]

  def_input_pad :input,
    demand_unit: :buffers,
    demand_mode: :auto,
    accepted_format: %H264{alignment: :au}

  def_output_pad :output,
    demand_mode: :auto,
    accepted_format: %RawVideo{pixel_format: format, aligned: true} when format in [:I420, :I422]

  @impl true
  def handle_init(_ctx, opts) do
    state = %{decoder_ref: nil, format_changed?: false, use_shm?: opts.use_shm?}
    {[], state}
  end

  @impl true
  def handle_setup(_ctx, state) do
    {[], %{state | decoder_ref: Native.create!()}}
  end

  @impl true
  def handle_process(:input, buffer, ctx, state) do
    %{decoder_ref: decoder_ref, use_shm?: use_shm?} = state

    dts = Common.to_h264_time_base_truncated(buffer.dts)
    pts = Common.to_h264_time_base_truncated(buffer.pts)

    case Native.decode(
           buffer.payload,
           pts,
           dts,
           use_shm?,
           decoder_ref
         ) do
      {:ok, pts_list_h264_base, frames} ->
        bufs = wrap_frames(pts_list_h264_base, frames)
        in_stream_format = ctx.pads.input.stream_format
        {stream_format, state} = update_stream_format_if_needed(state, in_stream_format)

        {stream_format ++ bufs, state}

      {:error, reason} ->
        raise "Native decoder failed to decode the payload: #{inspect(reason)}"
    end
  end

  @impl true
  def handle_stream_format(:input, _stream_format, _ctx, state) do
    # only redeclaring decoder - new stream_format will be generated in handle_process, after decoding key_frame
    {[], %{state | decoder_ref: Native.create!(), format_changed?: true}}
  end

  @impl true
  def handle_end_of_stream(:input, _ctx, state) do
    with {:ok, best_effort_pts_list, frames} <-
           Native.flush(state.use_shm?, state.decoder_ref),
         bufs <- wrap_frames(best_effort_pts_list, frames) do
      actions = bufs ++ [end_of_stream: :output]
      {actions, state}
    else
      {:error, reason} -> raise "Native decoder failed to flush: #{inspect(reason)}"
    end
  end

  defp wrap_frames([], []), do: []

  defp wrap_frames(pts_list, frames) do
    Enum.zip(pts_list, frames)
    |> Enum.map(fn {pts, frame} ->
      %Buffer{pts: Common.to_membrane_time_base_truncated(pts), payload: frame}
    end)
    |> then(&[buffer: {:output, &1}])
  end

  defp update_stream_format_if_needed(
         %{format_changed?: true, decoder_ref: decoder_ref} = state,
         in_stream_format
       ) do
    {[stream_format: {:output, generate_stream_format(in_stream_format, decoder_ref)}],
     %{state | format_changed?: false}}
  end

  defp update_stream_format_if_needed(%{format_changed?: false} = state, _in_stream_format) do
    {[], state}
  end

  defp generate_stream_format(input_stream_format, decoder_ref) do
    {:ok, width, height, pix_fmt} = Native.get_metadata(decoder_ref)

    framerate =
      case input_stream_format do
        nil -> {0, 1}
        %H264{framerate: in_framerate} -> in_framerate
      end

    %RawVideo{
      aligned: true,
      pixel_format: pix_fmt,
      framerate: framerate,
      height: height,
      width: width
    }
  end
end
