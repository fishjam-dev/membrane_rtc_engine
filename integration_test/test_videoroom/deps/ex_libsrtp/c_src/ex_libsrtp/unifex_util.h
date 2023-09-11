#pragma once

#include <unifex/payload.h>
#include <unifex/unifex.h>

UnifexPayload *unifex_payload_clone_ex(UnifexEnv *env, UnifexPayload *original,
                                       UnifexPayloadType type,
                                       unsigned int size);
UnifexPayload *unifex_payload_clone(UnifexEnv *env, UnifexPayload *original);

void unifex_payload_free_clone(UnifexPayload *payload);
