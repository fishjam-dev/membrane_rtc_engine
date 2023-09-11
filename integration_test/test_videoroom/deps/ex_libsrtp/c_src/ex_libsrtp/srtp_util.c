#include "srtp_util.h"
#include <string.h>

bool srtp_util_unmarshal_ssrc(int ssrc_type, unsigned int ssrc,
                              srtp_ssrc_t *result) {
  switch (ssrc_type) {
  case ssrc_specific:
    result->type = ssrc_specific;
    result->value = ssrc;
    return true;
  case ssrc_any_inbound:
    result->type = ssrc_any_inbound;
    return true;
  case ssrc_any_outbound:
    result->type = ssrc_any_outbound;
    return true;
  default:
    return false;
  }
}

bool srtp_util_set_crypto_policy_from_crypto_profile_atom(
    char *crypto_profile, srtp_crypto_policy_t *policy) {
  if (strcmp(crypto_profile, "rtp_default") == 0) {
    srtp_crypto_policy_set_rtp_default(policy);
    return true;
  }

  if (strcmp(crypto_profile, "rtcp_default") == 0) {
    srtp_crypto_policy_set_rtcp_default(policy);
    return true;
  }

  if (strcmp(crypto_profile, "aes_cm_128_hmac_sha1_80") == 0) {
    srtp_crypto_policy_set_aes_cm_128_hmac_sha1_80(policy);
    return true;
  }

  if (strcmp(crypto_profile, "aes_cm_128_hmac_sha1_32") == 0) {
    srtp_crypto_policy_set_aes_cm_128_hmac_sha1_32(policy);
    return true;
  }

  if (strcmp(crypto_profile, "aes_cm_128_null_auth") == 0) {
    srtp_crypto_policy_set_aes_cm_128_null_auth(policy);
    return true;
  }

  if (strcmp(crypto_profile, "null_cipher_hmac_sha1_80") == 0) {
    srtp_crypto_policy_set_null_cipher_hmac_sha1_80(policy);
    return true;
  }

  if (strcmp(crypto_profile, "null_cipher_hmac_null") == 0) {
    srtp_crypto_policy_set_null_cipher_hmac_null(policy);
    return true;
  }

  if (strcmp(crypto_profile, "aes_cm_256_hmac_sha1_80") == 0) {
    srtp_crypto_policy_set_aes_cm_256_hmac_sha1_80(policy);
    return true;
  }

  if (strcmp(crypto_profile, "aes_cm_256_hmac_sha1_32") == 0) {
    srtp_crypto_policy_set_aes_cm_256_hmac_sha1_32(policy);
    return true;
  }

  if (strcmp(crypto_profile, "aes_cm_256_null_auth") == 0) {
    srtp_crypto_policy_set_aes_cm_256_null_auth(policy);
    return true;
  }

  if (strcmp(crypto_profile, "aes_cm_192_hmac_sha1_80") == 0) {
    srtp_crypto_policy_set_aes_cm_192_hmac_sha1_80(policy);
    return true;
  }

  if (strcmp(crypto_profile, "aes_cm_192_hmac_sha1_32") == 0) {
    srtp_crypto_policy_set_aes_cm_192_hmac_sha1_32(policy);
    return true;
  }

  if (strcmp(crypto_profile, "aes_cm_192_null_auth") == 0) {
    srtp_crypto_policy_set_aes_cm_192_null_auth(policy);
    return true;
  }

  if (strcmp(crypto_profile, "aes_gcm_128_8_auth") == 0) {
    srtp_crypto_policy_set_aes_gcm_128_8_auth(policy);
    return true;
  }

  if (strcmp(crypto_profile, "aes_gcm_256_8_auth") == 0) {
    srtp_crypto_policy_set_aes_gcm_256_8_auth(policy);
    return true;
  }

  if (strcmp(crypto_profile, "aes_gcm_128_8_only_auth") == 0) {
    srtp_crypto_policy_set_aes_gcm_128_8_only_auth(policy);
    return true;
  }

  if (strcmp(crypto_profile, "aes_gcm_256_8_only_auth") == 0) {
    srtp_crypto_policy_set_aes_gcm_256_8_only_auth(policy);
    return true;
  }

  if (strcmp(crypto_profile, "aes_gcm_128_16_auth") == 0) {
    srtp_crypto_policy_set_aes_gcm_128_16_auth(policy);
    return true;
  }

  if (strcmp(crypto_profile, "aes_gcm_256_16_auth") == 0) {
    srtp_crypto_policy_set_aes_gcm_256_16_auth(policy);
    return true;
  }

  return false;
}

const char *srtp_util_strerror(srtp_err_status_t err) {
  switch (err) {
  case srtp_err_status_ok:
    return "srtp: nothing to report";
  case srtp_err_status_fail:
    return "srtp: unspecified failure";
  case srtp_err_status_bad_param:
    return "srtp: unsupported parameter";
  case srtp_err_status_alloc_fail:
    return "srtp: couldn't allocate memory";
  case srtp_err_status_dealloc_fail:
    return "srtp: couldn't deallocate properly";
  case srtp_err_status_init_fail:
    return "srtp: couldn't initialize";
  case srtp_err_status_terminus:
    return "srtp: can't process as much data as requested";
  case srtp_err_status_auth_fail:
    return "srtp: authentication failure";
  case srtp_err_status_cipher_fail:
    return "srtp: cipher failure";
  case srtp_err_status_replay_fail:
    return "srtp: replay check failed (bad index)";
  case srtp_err_status_replay_old:
    return "srtp: replay check failed (index too old)";
  case srtp_err_status_algo_fail:
    return "srtp: algorithm failed test routine";
  case srtp_err_status_no_such_op:
    return "srtp: unsupported operation";
  case srtp_err_status_no_ctx:
    return "srtp: no appropriate context found";
  case srtp_err_status_cant_check:
    return "srtp: unable to perform desired validation";
  case srtp_err_status_key_expired:
    return "srtp: can't use key any more";
  case srtp_err_status_socket_err:
    return "srtp: error in use of socket";
  case srtp_err_status_signal_err:
    return "srtp: error in use POSIX signals";
  case srtp_err_status_nonce_bad:
    return "srtp: nonce check failed";
  case srtp_err_status_read_fail:
    return "srtp: couldn't read data";
  case srtp_err_status_write_fail:
    return "srtp: couldn't write data";
  case srtp_err_status_parse_err:
    return "srtp: error parsing data";
  case srtp_err_status_encode_err:
    return "srtp: error encoding data";
  case srtp_err_status_semaphore_err:
    return "srtp: error while using semaphores";
  case srtp_err_status_pfkey_err:
    return "srtp: error while using pfkey";
  case srtp_err_status_bad_mki:
    return "srtp: error MKI present in packet is invalid";
  case srtp_err_status_pkt_idx_old:
    return "srtp: packet index is too old to consider";
  case srtp_err_status_pkt_idx_adv:
    return "srtp: packet index advanced, reset needed";
  default:
    return "srtp: unknown error";
  }
}

const char *srtp_util_error_to_atom(srtp_err_status_t err) {
  switch (err) {
  case srtp_err_status_ok:
    // hardly an error, but let's leave it here for completeness sake
    return "ok";
  case srtp_err_status_fail:
    return "fail";
  case srtp_err_status_bad_param:
    return "bad_param";
  case srtp_err_status_alloc_fail:
    return "alloc_fail";
  case srtp_err_status_dealloc_fail:
    return "dealloc_fail";
  case srtp_err_status_init_fail:
    return "init_fail";
  case srtp_err_status_terminus:
    return "terminus";
  case srtp_err_status_auth_fail:
    return "auth_fail";
  case srtp_err_status_cipher_fail:
    return "cipher_fail";
  case srtp_err_status_replay_fail:
    return "replay_fail";
  case srtp_err_status_replay_old:
    return "replay_old";
  case srtp_err_status_algo_fail:
    return "algo_fail";
  case srtp_err_status_no_such_op:
    return "no_such_op";
  case srtp_err_status_no_ctx:
    return "no_ctx";
  case srtp_err_status_cant_check:
    return "cant_check";
  case srtp_err_status_key_expired:
    return "key_expired";
  case srtp_err_status_socket_err:
    return "socket_err";
  case srtp_err_status_signal_err:
    return "signal_err";
  case srtp_err_status_nonce_bad:
    return "nonce_bad";
  case srtp_err_status_read_fail:
    return "read_fail";
  case srtp_err_status_write_fail:
    return "write_fail";
  case srtp_err_status_parse_err:
    return "parse_err";
  case srtp_err_status_encode_err:
    return "encode_err";
  case srtp_err_status_semaphore_err:
    return "semaphore_err";
  case srtp_err_status_pfkey_err:
    return "pfkey_err";
  case srtp_err_status_bad_mki:
    return "bad_mki";
  case srtp_err_status_pkt_idx_old:
    return "pkt_idx_old";
  case srtp_err_status_pkt_idx_adv:
    return "pkt_idx_adv";
  default:
    return "unknown";
  }
}
