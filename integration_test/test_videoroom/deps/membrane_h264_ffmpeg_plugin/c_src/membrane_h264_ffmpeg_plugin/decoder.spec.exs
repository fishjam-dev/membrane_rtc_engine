module Membrane.H264.FFmpeg.Decoder.Native

state_type "State"

spec create() :: {:ok :: label, state} | {:error :: label, reason :: atom}

spec decode(payload, pts :: int64, dts :: int64, use_shm :: bool, state) ::
       {:ok :: label, pts_list :: [int64], frames :: [payload]}
       | {:error :: label, reason :: atom}

spec flush(use_shm :: bool, state) ::
       {:ok :: label, pts_list :: [int64], frames :: [payload]}
       | {:error :: label, reason :: atom}

spec get_metadata(state) ::
       {:ok :: label, width :: int, height :: int, pix_fmt :: atom}
       | {:error :: label, :pix_fmt :: label}

dirty :cpu, decode: 3, flush: 1
