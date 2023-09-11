defmodule Membrane.Native.RawVideo do
  @moduledoc """
  Unifex-compatible implementation of the `Membrane.RawVideo` struct.

  Currently, unifex does not support tuples in the structs, so we have to decompose the framerate into two fields.
  """

  @typedoc """
  A numerator of the number of frames per second. To avoid using tuple type,
  it is described by 2 separate integers number.
  """
  @type framerate_num_t :: non_neg_integer

  @typedoc """
  The denominator of the number of frames per second. To avoid using tuple type,
  it is described by 2 separate integers number. The default value is 1.
  """
  @type framerate_den_t :: pos_integer

  @type t :: %__MODULE__{
          width: Membrane.RawVideo.width_t(),
          height: Membrane.RawVideo.height_t(),
          pixel_format: Membrane.RawVideo.pixel_format_t(),
          framerate_num: framerate_num_t(),
          framerate_den: framerate_den_t(),
          aligned: Membrane.RawVideo.aligned_t()
        }
  @enforce_keys [:width, :height, :pixel_format, :framerate_num, :aligned]
  defstruct width: nil,
            height: nil,
            pixel_format: nil,
            framerate_num: nil,
            framerate_den: 1,
            aligned: nil

  @doc """
  Creates unifex compatible struct from Membrane.RawVideo struct.
  It may raise error when RawVideo with not supported pixel format is provided.
  """
  @spec from_membrane_raw_video(Membrane.RawVideo.t()) :: t()

  def from_membrane_raw_video(%Membrane.RawVideo{} = membrane_raw_video) do
    {framerate_num, framerate_den} = membrane_raw_video.framerate

    %__MODULE__{
      width: membrane_raw_video.width,
      height: membrane_raw_video.height,
      pixel_format: membrane_raw_video.pixel_format,
      framerate_num: framerate_num,
      framerate_den: framerate_den,
      aligned: membrane_raw_video.aligned
    }
  end

  @doc """
  Convert a native raw video to the Membrane.RawVideo counterpart.
  """
  @spec to_membrane_raw_video(t()) :: Membrane.RawVideo.t()
  def to_membrane_raw_video(native_raw_video) do
    %Membrane.RawVideo{
      width: native_raw_video.width,
      height: native_raw_video.height,
      pixel_format: native_raw_video.pixel_format,
      framerate: {native_raw_video.framerate_num, native_raw_video.framerate_den},
      aligned: native_raw_video.aligned
    }
  end
end
