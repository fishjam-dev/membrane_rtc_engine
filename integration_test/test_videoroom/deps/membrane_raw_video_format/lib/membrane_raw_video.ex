defmodule Membrane.RawVideo do
  @moduledoc """
  This module provides a struct (`t:#{inspect(__MODULE__)}.t/0`) describing raw video frames.
  """
  require Integer

  alias Membrane.Native.RawVideo, as: NativeRawVideo

  @typedoc """
  Width of single frame in pixels.
  """
  @type width_t :: pos_integer()

  @typedoc """
  Height of single frame in pixels.
  """
  @type height_t :: pos_integer()

  @typedoc """
  Number of frames per second. To avoid using floating point numbers,
  it is described by 2 integers number of frames per timeframe in seconds.

  For example, NTSC's framerate of ~29.97 fps is represented by `{30_000, 1001}`
  """
  @type framerate_t :: {frames :: non_neg_integer, seconds :: pos_integer}

  @typedoc """
  Format used to encode the color of every pixel in each video frame.
  """
  @type pixel_format_t ::
          :I420 | :I422 | :I444 | :RGB | :BGRA | :RGBA | :NV12 | :NV21 | :YV12 | :AYUV | :YUY2

  @typedoc """
  Determines, whether buffers are aligned i.e. each buffer contains one frame.
  """
  @type aligned_t :: boolean()

  @type t :: %__MODULE__{
          width: width_t(),
          height: height_t(),
          framerate: framerate_t(),
          pixel_format: pixel_format_t(),
          aligned: aligned_t()
        }

  @enforce_keys [:width, :height, :framerate, :pixel_format, :aligned]
  defstruct @enforce_keys

  @supported_pixel_formats [
    :I420,
    :I422,
    :I444,
    :RGB,
    :BGRA,
    :RGBA,
    :NV12,
    :NV21,
    :YV12,
    :AYUV,
    :YUY2
  ]

  @doc """
  Simple wrapper over `frame_size/3`. Returns the size of raw video frame
  in bytes for the given caps.
  """
  @spec frame_size(t()) :: {:ok, pos_integer()} | {:error, reason}
        when reason: :invalid_dimensions | :invalid_pixel_format
  def frame_size(%__MODULE__{pixel_format: format, width: width, height: height}) do
    frame_size(format, width, height)
  end

  @doc """
  Returns the size of raw video frame in bytes (without padding).

  It may result in error when dimensions don't fulfill requirements for the given format
  (e.g. I420 requires both dimensions to be divisible by 2).
  """
  @spec frame_size(pixel_format_t(), width_t(), height_t()) ::
          {:ok, pos_integer()} | {:error, reason}
        when reason: :invalid_dimensions | :invalid_pixel_format
  def frame_size(format, width, height)
      when format in [:I420, :YV12, :NV12, :NV21] and Integer.is_even(width) and
             Integer.is_even(height) do
    # Subsampling by 2 in both dimensions
    # Y = width * height
    # V = U = (width / 2) * (height / 2)
    {:ok, div(width * height * 3, 2)}
  end

  def frame_size(format, width, height)
      when format in [:I422, :YUY2] and Integer.is_even(width) do
    # Subsampling by 2 in horizontal dimension
    # Y = width * height
    # V = U = (width / 2) * height
    {:ok, width * height * 2}
  end

  def frame_size(format, width, height) when format in [:I444, :RGB] do
    # No subsampling
    {:ok, width * height * 3}
  end

  def frame_size(format, width, height) when format in [:AYUV, :RGBA, :BGRA] do
    # No subsampling and added alpha channel
    {:ok, width * height * 4}
  end

  def frame_size(format, _width, _height) when format in @supported_pixel_formats do
    {:error, :invalid_dimensions}
  end

  def frame_size(_format, _width, _height) do
    {:error, :invalid_pixel_format}
  end

  @doc """
  Creates unifex-compatible struct from Membrane.RawVideo struct.
  Raises an error when RawVideo with an unsupported pixel format is provided.
  """
  @spec to_native_raw_video(t()) :: NativeRawVideo.t()
  def to_native_raw_video(%__MODULE__{} = membrane_raw_video) do
    NativeRawVideo.from_membrane_raw_video(membrane_raw_video)
  end

  @doc """
  Convert back a native raw video to the Membrane.RawVideo counterpart.
  """
  @spec from_native_raw_video(NativeRawVideo.t()) :: t()
  def from_native_raw_video(%NativeRawVideo{} = native_raw_video) do
    NativeRawVideo.to_membrane_raw_video(native_raw_video)
  end
end
