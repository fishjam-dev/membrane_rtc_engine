defmodule Membrane.H264.RemoteStream do
  @moduledoc """
  Module providing format definition for packetized, remote H264 video streams.

  Examples of such a stream:
  * H264 depayloaded from a container like FLV, where
  decoder configuration is signalled outside of the H264 bytestream.
  * H264 depayloaded from an RTP stream which is always aligned to
  NAL units.
  """
  @enforce_keys [:alignment]
  defstruct @enforce_keys ++ [:decoder_configuration_record]

  @typedoc """
  Format definition for packetized, remote H264 video streams.

  Regardless of the `alignment` value, NAL units are always in the Annex B format.

  In Annex B (defined in ITU-T H.264 Recommendation](http://www.itu.int/rec/T-REC-H.264-201704-I/en))
  each NAL unit is preceded by three or four-byte start code (`0x(00)000001`)
  that helps to identify boundaries.
  Annex B is suitable for writing to a file or streaming with MPEG-TS.
  """
  @type t() :: %__MODULE__{
          alignment: Membrane.H264.alignment_t(),
          decoder_configuration_record: binary() | nil
        }
end
