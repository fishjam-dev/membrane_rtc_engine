#include "srtp.h"
#include "srtp_util.h"
#include "unifex_util.h"

#include <stdbool.h>

#ifdef _WIN32
#include <winsock.h>
#else
#include <arpa/inet.h>
#endif

#define EXCEPTION_MESSAGE_SIZE 512

int on_load(UnifexEnv *env, void **priv_data) {
  UNIFEX_UNUSED(env);
  UNIFEX_UNUSED(priv_data);

  srtp_init();

  return 0;
}

void on_unload(UnifexEnv *env, void *priv_data) {
  UNIFEX_UNUSED(env);
  UNIFEX_UNUSED(priv_data);

  srtp_shutdown();
}

void handle_destroy_state(UnifexEnv *env, UnifexState *state) {
  if (state->session) {
    srtp_dealloc(state->session);
    state->session = NULL;
  }

  unifex_release_state(env, state);
}

static void free_master_keys_array(srtp_policy_t *policy) {
  if (policy->keys && policy->num_master_keys > 0) {
    for (size_t i = 0; i < policy->num_master_keys; i++) {
      srtp_master_key_t *key = policy->keys[i];
      unifex_free(key);
    }

    unifex_free(policy->keys);
    policy->keys = NULL;
    policy->num_master_keys = 0;
  }
}

static bool create_master_keys_array(UnifexEnv *env, UnifexPayload **keys,
                                     unsigned int keys_length,
                                     UnifexPayload **keys_mkis,
                                     unsigned int keys_mkis_length,
                                     srtp_policy_t *policy, UNIFEX_TERM *err) {
  if (keys_length == 0) {
    *err = unifex_raise_args_error(env, "keys", "must not be empty");
    goto create_master_keys_array_error;
  }

  // Validate key length
  for (size_t i = 0; i < keys_length; i++) {
    unsigned int expected_length = policy->rtp.cipher_key_len;
    if (keys[i]->size != expected_length) {
      char message[EXCEPTION_MESSAGE_SIZE];
      snprintf(message, EXCEPTION_MESSAGE_SIZE,
               "srtp: master key #%zu must have length of %d but has %d", i,
               expected_length, keys[i]->size);
      *err = unifex_raise(env, message);
      goto create_master_keys_array_error;
    }
  }

  // Single master key scenario
  if (keys_length == 1 && keys_mkis_length == 0) {
    policy->key = keys[0]->data;
    return true;
  }

  if (keys_length != keys_mkis_length) {
    *err = unifex_raise_args_error(env, "keys_mkis",
                                   "if there are multiple keys, must be of "
                                   "same length as keys, 0 otherwise");
    goto create_master_keys_array_error;
  }

  policy->keys = unifex_alloc(sizeof(srtp_master_key_t *) * keys_length);
  policy->num_master_keys = keys_length;
  if (!policy->keys) {
    *err = unifex_raise(env, "not enough memory");
    goto create_master_keys_array_error;
  }

  // Set all pointers to NULL
  memset(policy->keys, 0, sizeof(srtp_master_key_t *) * keys_length);

  for (size_t i = 0; i < keys_length; i++) {
    srtp_master_key_t *key = unifex_alloc(sizeof(srtp_master_key_t));
    if (!key) {
      *err = unifex_raise(env, "not enough memory");
      goto create_master_keys_array_error;
    }

    key->key = keys[i]->data;
    key->mki_id = keys_mkis[i]->data;
    key->mki_size = keys_mkis[i]->size;

    policy->keys[i] = key;
  }

  return true;

create_master_keys_array_error:
  free_master_keys_array(policy);
  return false;
}

UNIFEX_TERM create(UnifexEnv *env) {
  UnifexState *state = unifex_alloc_state(env);
  state->session = NULL;

  srtp_err_status_t err = srtp_create(&state->session, NULL);
  if (err) {
    unifex_release_state(env, state);
    return unifex_raise(env, srtp_util_strerror(err));
  }

  return create_result(env, state);
}

bool create_policy(UnifexEnv *env, srtp_policy_t *policy, int ssrc_type,
                   unsigned ssrc, UnifexPayload **keys,
                   unsigned int keys_length, UnifexPayload **keys_mkis,
                   unsigned int keys_mkis_length, char *rtp_crypto_profile,
                   char *rtcp_crypto_profile, unsigned window_size,
                   int allow_repeat_tx, UNIFEX_TERM *unifex_error) {
  int err;

  memset(policy, 0, sizeof(srtp_policy_t));

  policy->window_size = window_size;
  policy->allow_repeat_tx = allow_repeat_tx;

  err = srtp_util_unmarshal_ssrc(ssrc_type, ssrc, &policy->ssrc);
  if (!err) {
    *unifex_error = unifex_raise_args_error(env, "ssrc", "invalid");
    return false;
  }

  err = srtp_util_set_crypto_policy_from_crypto_profile_atom(rtp_crypto_profile,
                                                             &policy->rtp);
  if (!err) {
    *unifex_error = unifex_raise_args_error(env, "rtp", "invalid");
    return false;
  }

  err = srtp_util_set_crypto_policy_from_crypto_profile_atom(
      rtcp_crypto_profile, &policy->rtcp);
  if (!err) {
    *unifex_error = unifex_raise_args_error(env, "rtcp", "invalid");
    return false;
  }

  err = create_master_keys_array(env, keys, keys_length, keys_mkis,
                                 keys_mkis_length, policy, unifex_error);
  if (!err) {
    return false;
  }

  return true;
}

UNIFEX_TERM add_stream(UnifexEnv *env, UnifexState *state, int ssrc_type,
                       unsigned ssrc, UnifexPayload **keys,
                       unsigned int keys_length, UnifexPayload **keys_mkis,
                       unsigned int keys_mkis_length, char *rtp_crypto_profile,
                       char *rtcp_crypto_profile, unsigned window_size,
                       int allow_repeat_tx) {
  srtp_err_status_t serr;
  UNIFEX_TERM unifex_error;

  srtp_policy_t policy;
  bool ret =
      create_policy(env, &policy, ssrc_type, ssrc, keys, keys_length, keys_mkis,
                    keys_mkis_length, rtp_crypto_profile, rtcp_crypto_profile,
                    window_size, allow_repeat_tx, &unifex_error);
  if (!ret) {
    return unifex_error;
  }

  serr = srtp_add_stream(state->session, &policy);

  free_master_keys_array(&policy);

  if (serr) {
    return unifex_raise(env, srtp_util_strerror(serr));
  }

  return add_stream_result_ok(env);
}

UNIFEX_TERM remove_stream(UnifexEnv *env, UnifexState *state, unsigned ssrc) {
  // htonl below is required due to legacy reasons, see
  // https://github.com/cisco/libsrtp/issues/306
  srtp_err_status_t serr = srtp_remove_stream(state->session, htonl(ssrc));
  if (serr) {
    return unifex_raise(env, srtp_util_strerror(serr));
  }

  return remove_stream_result_ok(env);
}

UNIFEX_TERM update(UnifexEnv *env, UnifexState *state, int ssrc_type,
                   unsigned ssrc, UnifexPayload **keys,
                   unsigned int keys_length, UnifexPayload **keys_mkis,
                   unsigned int keys_mkis_length, char *rtp_crypto_profile,
                   char *rtcp_crypto_profile, unsigned window_size,
                   int allow_repeat_tx) {
  UNIFEX_TERM unifex_error;
  srtp_policy_t policy;

  bool ret =
      create_policy(env, &policy, ssrc_type, ssrc, keys, keys_length, keys_mkis,
                    keys_mkis_length, rtp_crypto_profile, rtcp_crypto_profile,
                    window_size, allow_repeat_tx, &unifex_error);
  if (!ret) {
    return unifex_error;
  }

  srtp_err_status_t serr = srtp_update(state->session, &policy);
  free_master_keys_array(&policy);
  if (serr) {
    return unifex_raise(env, srtp_util_strerror(serr));
  }

  return update_result_ok(env);
}

UNIFEX_TERM protect(UnifexEnv *env, UnifexState *state, char *what,
                    UnifexPayload *payload, int use_mki, unsigned mki_index) {
  int err;
  srtp_err_status_t serr;

  UnifexPayload *protected = unifex_payload_clone_ex(
      env, payload, payload->type, payload->size + SRTP_MAX_TRAILER_LEN);
  int len = (int)payload->size;

  serr = !strcmp(what, "rtp")
             ? srtp_protect_mki(state->session, protected->data, &len, use_mki,
                                mki_index)
             : srtp_protect_rtcp_mki(state->session, protected->data, &len,
                                     use_mki, mki_index);
  if (serr) {
    unifex_payload_free_clone(protected);
    return protect_result_error(env, srtp_util_error_to_atom(serr));
  }

  err = unifex_payload_realloc(protected, len);
  if (!err) {
    unifex_payload_free_clone(protected);
    return unifex_raise(env, "failed to realloc protected payload");
  }

  UNIFEX_TERM res = protect_result_ok(env, protected);
  unifex_payload_free_clone(protected);
  return res;
}

UNIFEX_TERM unprotect(UnifexEnv *env, UnifexState *state, char *what,
                      UnifexPayload *payload, int use_mki) {
  int err;
  srtp_err_status_t serr;

  UnifexPayload *unprotected = unifex_payload_clone(env, payload);
  int len = (int)payload->size;

  serr =
      !strcmp(what, "rtp")
          ? srtp_unprotect_mki(state->session, unprotected->data, &len, use_mki)
          : srtp_unprotect_rtcp_mki(state->session, unprotected->data, &len,
                                    use_mki);
  if (serr) {
    unifex_payload_free_clone(unprotected);
    return unprotect_result_error(env, srtp_util_error_to_atom(serr));
  }

  err = unifex_payload_realloc(unprotected, len);
  if (!err) {
    unifex_payload_free_clone(unprotected);
    return unifex_raise(env, "failed to realloc unprotected payload");
  }

  UNIFEX_TERM res = unprotect_result_ok(env, unprotected);
  unifex_payload_free_clone(unprotected);
  return res;
}
