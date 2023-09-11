module ExLibSRTP.Native

state_type "State"

callback :load, :on_load
callback :unload, :on_unload

spec create() :: state

spec add_stream(
       state,
       ssrc_type :: int,
       ssrc :: unsigned,
       keys :: [payload],
       keys_mkis :: [payload],
       rtp_crypto_profile :: atom,
       rtcp_crypto_profile :: atom,
       window_size :: unsigned,
       allow_repeat_tx :: bool
     ) :: :ok :: label

spec remove_stream(state, ssrc :: unsigned) :: :ok :: label

spec update(
       state,
       ssrc_type :: int,
       ssrc :: unsigned,
       keys :: [payload],
       keys_mkis :: [payload],
       rtp_crypto_profile :: atom,
       rtcp_crypto_profile :: atom,
       window_size :: unsigned,
       allow_repeat_tx :: bool
     ) :: :ok :: label

spec protect(state, what :: atom, payload, use_mki :: bool, mki_index :: unsigned) ::
       {:ok :: label, payload}
       | {:error :: label, reason :: atom}

spec unprotect(state, what :: atom, payload, use_mki :: bool) ::
       {:ok :: label, payload}
       | {:error :: label, reason :: atom}
