module Membrane.H264.FFmpeg.Parser.Native

state_type "State"

type(
  resolution :: %Resolution{
    width: int,
    height: int,
    index: int
  }
)

spec create() :: {:ok :: label, state} | {:error :: label, reason :: atom}

spec parse(payload, state) ::
       {:ok :: label, frame_sizes :: [unsigned], decoding_order_numbers :: [int],
        presentation_order_numbers :: [int], resolutions :: [resolution]}
       | {:error :: label, reason :: atom}

spec get_profile(state) :: {:ok :: label, profile :: atom}

spec flush(state) ::
       {:ok :: label, frame_sizes :: [unsigned], decoding_order_number :: [int],
        presentation_order_number :: [int], resolution :: resolution}
       | {:error :: label, reason :: atom}
