#pragma once

#include "dtls.h"
#include <unifex/unifex.h>

typedef struct State State;

struct State {
  UnifexEnv *env;
  SSL_CTX *ssl_ctx;
  SSL *ssl;
  EVP_PKEY *pkey;
  X509 *x509;
  int client_mode;
  int hsk_finished;
};

#include "_generated/native.h"
