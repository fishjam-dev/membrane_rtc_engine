#include "payload.h"

ErlNifResourceType *UNIFEX_PAYLOAD_GUARD_RESOURCE_TYPE = NULL;

int unifex_payload_from_term(ErlNifEnv *env, ERL_NIF_TERM term,
                             UnifexPayload *payload) {
  int res = enif_inspect_binary(env, term, &payload->payload_struct.binary);
  if (res) {
    payload->data = payload->payload_struct.binary.data;
    payload->size = payload->payload_struct.binary.size;
    payload->type = UNIFEX_PAYLOAD_BINARY;
    payload->owned = 0;
    return res;
  }

  res = shmex_get_from_term(env, term, &payload->payload_struct.shm);
  if (res) {
    shmex_open_and_mmap(&payload->payload_struct.shm);
    payload->data = payload->payload_struct.shm.mapped_memory;
    payload->size = payload->payload_struct.shm.capacity;
    payload->type = UNIFEX_PAYLOAD_SHM;
    payload->owned = 1;
  }
  return res;
}

UNIFEX_TERM unifex_payload_to_term(ErlNifEnv *env, UnifexPayload *payload) {
  switch (payload->type) {
  case UNIFEX_PAYLOAD_BINARY:
    payload->owned = 0;
    return enif_make_binary(env, &payload->payload_struct.binary);
  case UNIFEX_PAYLOAD_SHM:
    return shmex_make_term(env, &payload->payload_struct.shm);
  }
  // Switch should be exhaustive, this is added just to silence the warning
  return enif_raise_exception(env,
                              enif_make_atom(env, "unifex_payload_to_term"));
}

int unifex_payload_alloc(UnifexEnv *env, UnifexPayloadType type,
                                    unsigned int size, UnifexPayload *payload) {
  Shmex *p_struct;
  ShmexLibResult result;

  switch (type) {
  case UNIFEX_PAYLOAD_BINARY:
    if (enif_alloc_binary(size, &payload->payload_struct.binary) != 1) {
        return 0;
    }
    payload->data = payload->payload_struct.binary.data;
    break;
  case UNIFEX_PAYLOAD_SHM:
    p_struct = &payload->payload_struct.shm;
    shmex_init(env, p_struct, size);
    result = shmex_allocate(env, UNIFEX_PAYLOAD_GUARD_RESOURCE_TYPE, p_struct);
    if (SHMEX_RES_OK != result) {
      return 0;
    }
    result = shmex_open_and_mmap(p_struct);
    if (SHMEX_RES_OK != result) {
      shmex_release(p_struct);
      return 0;
    }
    p_struct->size = size;
    payload->data = p_struct->mapped_memory;
    break;
  }

  payload->size = size;
  payload->type = type;
  payload->owned = 1;

  return 1;
}

void unifex_payload_guard_destructor(UnifexEnv *env, void *resource) {
  shmex_guard_destructor(env, resource);
}

int unifex_payload_realloc(UnifexPayload *payload, unsigned int size) {
  int res = 1;
  payload->size = size;
  Shmex *shmex;

  switch (payload->type) {
  case UNIFEX_PAYLOAD_BINARY:
    payload->owned = 1;
    res = enif_realloc_binary(&payload->payload_struct.binary, size);
    payload->data = payload->payload_struct.binary.data;
    break;
  case UNIFEX_PAYLOAD_SHM:
    shmex = &payload->payload_struct.shm;
    shmex_unmap(shmex);
    ShmexLibResult shmex_res = shmex_set_capacity(shmex, size);
    if (shmex_res != SHMEX_RES_OK) {
      return 0;
    }
    shmex_res = shmex_open_and_mmap(shmex);
    if (shmex_res != SHMEX_RES_OK) {
      return 0;
    }
    payload->payload_struct.shm.size = payload->size;
    break;
  }
  return res;
}

void unifex_payload_release(UnifexPayload *payload) {
  switch (payload->type) {
  case UNIFEX_PAYLOAD_BINARY:
    if (payload->owned) {
      enif_release_binary(&payload->payload_struct.binary);
    }
    break;
  case UNIFEX_PAYLOAD_SHM:
    shmex_release(&payload->payload_struct.shm);
    break;
  }
}
