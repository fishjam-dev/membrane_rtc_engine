#pragma once

#include <srtp2/srtp.h>

typedef struct State {
  srtp_t session;
} State;

#include "_generated/srtp.h"
