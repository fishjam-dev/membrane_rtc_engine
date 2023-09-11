defmodule ExLibSRTP.Policy do
  @moduledoc """
  Policy for setting up SRTP stream configuration.

  For meaning of particular fields maps to the fields of `srtp_policy_t` C struct found in libSRTP.
  It is described in [srtp.h header](https://github.com/cisco/libsrtp/blob/43dab118f7acbd471edd68ca16e23ed7075dbd38/include/srtp.h#L285)

  Here's a brief description:
    * `:ssrc` - Either an accepted SSRC or atoms mapping to `SSRC_ANY_(INBOUND/OUTBOUND)` flags
    * `:key` - a master key for encrpytion
    * `:rtp` - crypto profile defining a policy for RTP encryption
    * `:rtcp` - crypto profile defining a policy for RTCP encryption
    * `:windows_size` - the sequence number window size used for replay protection
      As the comment [here](https://github.com/cisco/libsrtp/blob/43dab118f7acbd471edd68ca16e23ed7075dbd38/srtp/srtp.c#L1270) says,
      it must be at least 64 and any value above 2^15 (32768) won't be effective as explained [here](https://github.com/cisco/libsrtp/issues/470).
      The default value is 128
    * `:allow_repeat_tx` - if true, packet with repeated sequence number won't cause an error.
      Note that unless the RTP payload is the same it may introduce a severe security weakness.
  """

  # TODO: add EKT, enc_xtn_hdr

  alias ExLibSRTP.MasterKey

  @type ssrc_pattern_t :: ExLibSRTP.ssrc_t() | :any_inbound | :any_outbound

  @type crypto_profile_t ::
          :rtp_default
          | :rtcp_default
          | :aes_cm_128_hmac_sha1_80
          | :aes_cm_128_hmac_sha1_32
          | :aes_cm_128_null_auth
          | :null_cipher_hmac_sha1_80
          | :null_cipher_hmac_null
          | :aes_cm_256_hmac_sha1_80
          | :aes_cm_256_hmac_sha1_32
          | :aes_cm_256_null_auth
          | :aes_cm_192_hmac_sha1_80
          | :aes_cm_192_hmac_sha1_32
          | :aes_cm_192_null_auth
          | :aes_gcm_128_8_auth
          | :aes_gcm_256_8_auth
          | :aes_gcm_128_8_only_auth
          | :aes_gcm_256_8_only_auth
          | :aes_gcm_128_16_auth
          | :aes_gcm_256_16_auth

  @type key_spec_t :: binary() | [MasterKey.t()]

  @type t :: %__MODULE__{
          ssrc: ssrc_pattern_t,
          key: key_spec_t(),
          rtp: crypto_profile_t(),
          rtcp: crypto_profile_t(),
          window_size: 64..32_768 | :default,
          allow_repeat_tx: boolean()
        }

  @enforce_keys [:ssrc, :key]
  defstruct @enforce_keys ++
              [
                rtp: :rtp_default,
                rtcp: :rtcp_default,
                window_size: :default,
                allow_repeat_tx: false
              ]

  @doc """
  Relevant specification: https://www.iana.org/assignments/srtp-protection/srtp-protection.xhtml
  """
  @spec crypto_profile_from_dtls_srtp_protection_profile(
          value :: pos_integer() | {pos_integer(), pos_integer()}
        ) :: {:ok, crypto_profile_t()} | {:error, :unsupported_crypto_profile}
  def crypto_profile_from_dtls_srtp_protection_profile(0x01), do: {:ok, :aes_cm_128_hmac_sha1_80}
  def crypto_profile_from_dtls_srtp_protection_profile(0x02), do: {:ok, :aes_cm_128_hmac_sha1_32}
  def crypto_profile_from_dtls_srtp_protection_profile(0x05), do: {:ok, :null_cipher_hmac_sha1_80}
  # null_cipher_hmac_sha1_32 is not supported in libsrtp2
  def crypto_profile_from_dtls_srtp_protection_profile(0x06),
    do: {:error, :unsupported_crypto_profile}

  def crypto_profile_from_dtls_srtp_protection_profile(0x07), do: {:ok, :aes_gcm_128_16_auth}
  def crypto_profile_from_dtls_srtp_protection_profile(0x08), do: {:ok, :aes_gcm_256_16_auth}

  def crypto_profile_from_dtls_srtp_protection_profile(b) when is_number(b) do
    {:error, :unsupported_crypto_profile}
  end

  def crypto_profile_from_dtls_srtp_protection_profile({0x00, b}) when is_number(b) do
    crypto_profile_from_dtls_srtp_protection_profile(b)
  end

  def crypto_profile_from_dtls_srtp_protection_profile({a, b})
      when is_number(a) and is_number(b) do
    {:error, :unsupported_crypto_profile}
  end
end
