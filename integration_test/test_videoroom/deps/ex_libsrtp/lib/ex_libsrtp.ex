defmodule ExLibSRTP do
  @moduledoc """
  [libsrtp](https://github.com/cisco/libsrtp) bindings for Elixir.

  The workflow goes as follows:
  - create ExLibSRTP instance with `new/0`
  - add streams with `add_stream/2`
  - protect or unprotect packets with `protect/3`, `unprotect/3`, `protect_rtcp/3`, `unprotect_rtcp/3`
  - remove streams with `remove_stream/2`
  """
  require Record

  alias ExLibSRTP.{Native, Policy}

  @typedoc """
  Type describing possible errors that might be returned by ExLibSRTP functions.

  Meaning of these might vary depending on the function. For explanation, please refer to
  appropriate documentation.

  This type is based on [`srtp_err_status_t` enum](https://github.com/cisco/libsrtp/blob/004b7bb85caf4f52a7cc8a7aad94d9d1706e8b6d/include/srtp.h#L164).
  """
  @type libsrtp_error_t() ::
          :fail
          | :bad_param
          | :alloc_fail
          | :dealloc_fail
          | :init_fail
          | :terminus
          | :auth_fail
          | :cipher_fail
          | :replay_fail
          | :replay_old
          | :algo_fail
          | :no_such_op
          | :no_ctx
          | :cant_check
          | :key_expired
          | :socket_err
          | :signal_err
          | :nonce_bad
          | :read_fail
          | :write_fail
          | :parse_err
          | :encode_err
          | :semaphore_err
          | :pfkey_err
          | :bad_mki
          | :pkt_idx_old

  @opaque t :: {__MODULE__, native :: reference}

  @type ssrc_t :: 0..4_294_967_295

  defguard is_ssrc(ssrc) when ssrc in 0..4_294_967_295

  defmacrop ref(native) do
    quote do
      {unquote(__MODULE__), unquote(native)}
    end
  end

  @spec new() :: t()
  def new() do
    ref(Native.create())
  end

  @spec add_stream(t(), policy :: Policy.t()) :: :ok
  def add_stream(ref(native) = _srtp, %Policy{} = policy) do
    {ssrc_type, ssrc} = Native.marshal_ssrc(policy.ssrc)
    {keys, keys_mkis} = Native.marshal_master_keys(policy.key)
    window_size = Native.marshal_window_size(policy.window_size)

    Native.add_stream(
      native,
      ssrc_type,
      ssrc,
      keys,
      keys_mkis,
      policy.rtp,
      policy.rtcp,
      window_size,
      policy.allow_repeat_tx
    )
  end

  @spec remove_stream(t(), ssrc :: ssrc_t()) :: :ok
  def remove_stream(ref(native) = _srtp, ssrc) when is_ssrc(ssrc) do
    Native.remove_stream(native, ssrc)
  end

  @spec update(t(), policy :: Policy.t()) :: :ok
  def update(ref(native), %Policy{} = policy) do
    {ssrc_type, ssrc} = Native.marshal_ssrc(policy.ssrc)
    {keys, keys_mkis} = Native.marshal_master_keys(policy.key)
    window_size = Native.marshal_window_size(policy.window_size)

    Native.update(
      native,
      ssrc_type,
      ssrc,
      keys,
      keys_mkis,
      policy.rtp,
      policy.rtcp,
      window_size,
      policy.allow_repeat_tx
    )
  end

  @doc """
  Protect RTP packet.

  Most common errors:
  - `:replay_fail` - packet has either a duplicate sequence number or its sequence number has an old rollover counter (roc)
  - `:replay_old` - packet has a sequence number with current roc, but it is older than the beginning of the encryption window.
  - `:bad_mki` - provided MKI is not a known MKI id

  Other errors indicate a failure in cryptographic mechanisms, please refer to [libsrtp documentation](https://github.com/cisco/libsrtp)
  """
  @spec protect(t(), unprotected :: binary(), mki_index :: pos_integer() | nil) ::
          {:ok, protected :: binary()} | {:error, libsrtp_error_t()}
  def protect(srtp, unprotected, mki_index \\ nil)

  def protect(ref(native), unprotected, nil) do
    Native.protect(native, :rtp, unprotected, false, 0)
  end

  def protect(ref(native), unprotected, mki_index) when is_integer(mki_index) do
    Native.protect(native, :rtp, unprotected, true, mki_index)
  end

  @doc """
  Protect RTCP packet.

  All errors indicate an error in the cryptographic mechanisms.
  """
  @spec protect_rtcp(t(), unprotected :: binary(), mki_index :: pos_integer() | nil) ::
          {:ok, protected :: binary()} | {:error, libsrtp_error_t()}
  def protect_rtcp(srtp, unprotected, mki_index \\ nil)

  def protect_rtcp(ref(native), unprotected, nil) do
    Native.protect(native, :rtcp, unprotected, false, 0)
  end

  def protect_rtcp(ref(native), unprotected, mki_index) when is_integer(mki_index) do
    Native.protect(native, :rtcp, unprotected, true, mki_index)
  end

  @doc """
  Unprotect RTP packet.

  Most common errors:
  - `:replay_fail` - packet has either a duplicate sequence number or its sequence number has an old rollover counter (roc)
  - `:replay_old` - packet has a sequence number with current roc, but it is older than the beginning of the decryption window.
  - `:auth_fail` - packet has failed the message authentication check

  Other errors indicate a failure in cryptographic mechanisms, please refer to [libsrtp documentation](https://github.com/cisco/libsrtp)
  """
  @spec unprotect(t(), protected :: binary(), use_mki :: boolean()) ::
          {:ok, unprotected :: binary()} | {:error, libsrtp_error_t()}
  def unprotect(ref(native) = _srtp, protected, use_mki \\ false) do
    Native.unprotect(native, :rtp, protected, use_mki)
  end

  @doc """
  Unprotect RTCP packet.

  Expected errors:
  - `:auth_fail` - SRTCP message has failed the message authentication check.
  - `:replay_fail` - SRTCP message is a duplicate
  - `:bad_mki` - provided MKI is not a known MKI id

  Other errors indicate issues in the cryptographic mechanisms.
  """
  @spec unprotect_rtcp(t(), protected :: binary(), use_mki :: boolean()) ::
          {:ok, unprotected :: binary()} | {:error, libsrtp_error_t()}
  def unprotect_rtcp(ref(native) = _srtp, protected, use_mki \\ false) do
    Native.unprotect(native, :rtcp, protected, use_mki)
  end
end
