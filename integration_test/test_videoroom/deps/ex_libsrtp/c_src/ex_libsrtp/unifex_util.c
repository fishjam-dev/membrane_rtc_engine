#include "unifex_util.h"

UnifexPayload *unifex_payload_clone_ex(UnifexEnv *env, UnifexPayload *original,
                                       UnifexPayloadType type,
                                       unsigned int size) {
  UnifexPayload *payload = (UnifexPayload *)unifex_alloc(sizeof(UnifexPayload));
  unifex_payload_alloc(env, type, size, payload);
  size_t to_copy = size < original->size ? size : original->size;
  memcpy(payload->data, original->data, to_copy);
  return payload;
}

UnifexPayload *unifex_payload_clone(UnifexEnv *env, UnifexPayload *original) {
  return unifex_payload_clone_ex(env, original, original->type, original->size);
}

void unifex_payload_free_clone(UnifexPayload *payload) {
  unifex_payload_release(payload);
  unifex_free(payload);
}
