#include "payload.h"

int unifex_payload_decode(UnifexEnv *_env, UnifexCNodeInBuff *buff,
                          UnifexPayload **payload) {
  UNIFEX_UNUSED(_env);
  int type, size;
  if (ei_get_type(buff->buff, buff->index, &type, &size)) {
    return 1;
  }
  *payload = unifex_alloc(sizeof(UnifexPayload));
  **payload = (UnifexPayload){.data = unifex_alloc(size),
                              .size = (unsigned int)size,
                              .type = UNIFEX_PAYLOAD_BINARY,
                              .owned = 0};
  long len;
  if (ei_decode_binary(buff->buff, buff->index, (*payload)->data, &len)) {
    return 1;
  }
  return 0;
}

void unifex_payload_encode(UnifexEnv *env, UNIFEX_TERM buff,
                           UnifexPayload *payload) {
  switch (payload->type) {
  case UNIFEX_PAYLOAD_BINARY:
    ei_x_encode_binary(buff, payload->data, payload->size);
    return;
  }
  // Switch should be exhaustive
  env->error = unifex_raise(env, "unifex_payload_encode");
}

int unifex_payload_alloc(UnifexEnv *_env, UnifexPayloadType type,
                                    unsigned int size, UnifexPayload *payload) {
  UNIFEX_UNUSED(_env);

  switch (type) {
  case UNIFEX_PAYLOAD_BINARY:
    payload->data = unifex_alloc(size);
    if (!payload->data) {
      return 0;
    }
    break;
  }

  payload->type = type;
  payload->size = size;
  payload->owned = 1;

  return 1;
}

void unifex_payload_release(UnifexPayload *payload) {
  if (payload == NULL) {
    return;
  }

  switch (payload->type) {
  case UNIFEX_PAYLOAD_BINARY:
    if (payload->data) {
      unifex_free(payload->data);
    }
    break;
  }
}
