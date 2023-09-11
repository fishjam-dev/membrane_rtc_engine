#pragma once

#include <srtp2/srtp.h>
#include <stdbool.h>

const char *srtp_util_strerror(srtp_err_status_t err);

bool srtp_util_unmarshal_ssrc(int ssrc_type, unsigned int ssrc,
                              srtp_ssrc_t *result);

bool srtp_util_set_crypto_policy_from_crypto_profile_atom(
    char *crypto_profile, srtp_crypto_policy_t *policy);

const char *srtp_util_error_to_atom(srtp_err_status_t err);
