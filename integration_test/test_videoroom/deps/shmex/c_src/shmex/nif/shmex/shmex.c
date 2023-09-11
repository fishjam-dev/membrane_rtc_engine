#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>

#include "shmex.h"

/**
 * Initializes Shmex C struct. Should be used before allocating shm from C code.
 *
 * Each call should be paired with `shmex_release` call to deallocate resources.
 */
void shmex_init(ErlNifEnv *env, Shmex *payload, unsigned capacity) {
  payload->guard = enif_make_atom(env, "nil");
  payload->size = 0;
  payload->capacity = capacity;
  payload->mapped_memory = MAP_FAILED;
  payload->name = NULL;
}

/**
 * Allocates shared memory using `shmex_allocate_unguarded` and adds guard to it
 * with `shmex_add_guard`.
 */
ShmexLibResult shmex_allocate(ErlNifEnv *env, ErlNifResourceType *guard_type,
                              Shmex *payload) {
  ShmexLibResult result = shmex_allocate_unguarded(payload);
  if (SHMEX_RES_OK != result) {
    return result;
  }
  shmex_add_guard(env, guard_type, payload);
  return SHMEX_RES_OK;
}

/**
 * Creates a guard and adds it to given payload.
 *
 * Once the guard is garbage collected, the payload is freed.
 */
void shmex_add_guard(ErlNifEnv *env, ErlNifResourceType *guard_type,
                     Shmex *payload) {
  ShmexGuard *guard = enif_alloc_resource(guard_type, sizeof(*guard));
  strcpy(guard->name, payload->name);
  payload->guard = enif_make_resource(env, guard);
  enif_release_resource(guard);
}

/**
 * Destructor for payload guards.
 *
 * It is to be passed as a destructor to `enif_open_resource_type` function.
 */
void shmex_guard_destructor(ErlNifEnv *env, void *resource) {
  BUNCH_UNUSED(env);

  ShmexGuard *guard = (ShmexGuard *)resource;
  shmex_shm_unlink(guard->name);
}

/**
 * Initializes Shmex C struct using data from Shmex Elixir struct
 *
 * Each call should be paired with `shmex_release` call to deallocate resources
 */
int shmex_get_from_term(ErlNifEnv *env, ERL_NIF_TERM struct_term,
                        Shmex *payload) {
  const ERL_NIF_TERM ATOM_NAME = enif_make_atom(env, "name");
  const ERL_NIF_TERM ATOM_GUARD = enif_make_atom(env, "guard");
  const ERL_NIF_TERM ATOM_SIZE = enif_make_atom(env, "size");
  const ERL_NIF_TERM ATOM_CAPACITY = enif_make_atom(env, "capacity");

  int result;
  ERL_NIF_TERM tmp_term;

  payload->mapped_memory = MAP_FAILED;

  // Get guard
  result = enif_get_map_value(env, struct_term, ATOM_GUARD, &tmp_term);
  if (!result) {
    return 0;
  }
  payload->guard = tmp_term;

  // Get size
  result = enif_get_map_value(env, struct_term, ATOM_SIZE, &tmp_term);
  if (!result) {
    return 0;
  }
  result = enif_get_uint(env, tmp_term, &payload->size);
  if (!result) {
    return 0;
  }

  // Get capacity
  result = enif_get_map_value(env, struct_term, ATOM_CAPACITY, &tmp_term);
  if (!result) {
    return 0;
  }
  result = enif_get_uint(env, tmp_term, &payload->capacity);
  if (!result) {
    return 0;
  }

  // Get name as last to prevent failure after allocating memory
  result = enif_get_map_value(env, struct_term, ATOM_NAME, &tmp_term);
  if (!result) {
    return 0;
  }
  char atom_tmp[4];
  result = enif_get_atom(env, tmp_term, atom_tmp, 4, ERL_NIF_LATIN1);
  if (result) {
    if (strncmp(atom_tmp, "nil", 3) == 0) {
      payload->name = NULL;
      return 1;
    }

    return 0;
  }

  ErlNifBinary name_binary;
  result = enif_inspect_binary(env, tmp_term, &name_binary);
  if (!result) {
    return 0;
  }
  payload->name = malloc(name_binary.size + 1);
  memcpy(payload->name, (char *)name_binary.data, name_binary.size);
  payload->name[name_binary.size] = '\0';

  return 1;
}

/**
 * Deallocates resources owned by Shmex struct. It does not
 * free the actual shared memory segment, just object representing it.
 *
 * After calling this function, payload is not usable anymore.
 * If the payload was mapped, it is unmapped as well.
 */
void shmex_release(Shmex *payload) {
  if (payload->name != NULL) {
    free(payload->name);
    payload->name = NULL;
  }

  shmex_unmap(payload);
}

/**
 * Creates Shmex Elixir struct from Shmex C struct
 */
ERL_NIF_TERM shmex_make_term(ErlNifEnv *env, Shmex *payload) {
  ERL_NIF_TERM keys[SHMEX_ELIXIR_STRUCT_ENTRIES] = {
      enif_make_atom(env, "__struct__"), enif_make_atom(env, "name"),
      enif_make_atom(env, "guard"), enif_make_atom(env, "size"),
      enif_make_atom(env, "capacity")};

  ERL_NIF_TERM name_term;
  unsigned name_len = strlen(payload->name);
  void *name_ptr = enif_make_new_binary(env, name_len, &name_term);
  memcpy(name_ptr, payload->name, name_len);

  ERL_NIF_TERM values[SHMEX_ELIXIR_STRUCT_ENTRIES] = {
      enif_make_atom(env, SHMEX_ELIXIR_STRUCT_ATOM), name_term, payload->guard,
      enif_make_int(env, payload->size), enif_make_int(env, payload->capacity)};

  ERL_NIF_TERM return_term;
  int res = enif_make_map_from_arrays(
      env, keys, values, SHMEX_ELIXIR_STRUCT_ENTRIES, &return_term);
  if (res) {
    return return_term;
  } else {
    return bunch_raise_error(env, "cannot create Shmex struct");
  }
}

/**
 * Creates term describing an error encoded in result (ShmexLibResult)
 */
ERL_NIF_TERM shmex_make_error_term(ErlNifEnv *env, ShmexLibResult result) {
  switch (result) {
  case SHMEX_RES_OK:
    return bunch_raise_error(env, "ok_is_not_error");
  case SHMEX_ERROR_SHM_OPEN:
    return bunch_make_error_errno(env, "shm_open");
  case SHMEX_ERROR_FTRUNCATE:
    return bunch_make_error_errno(env, "ftruncate");
  case SHMEX_ERROR_MMAP:
    return bunch_make_error_errno(env, "mmap");
  case SHMEX_ERROR_SHM_MAPPED:
    return bunch_raise_error(env, "shm_is_mapped");
  case SHMEX_ERROR_INVALID_PAYLOAD:
    return bunch_make_error_str(env, "invalid_payload");
  default:
    return bunch_raise_error(env, "unknown");
  }
}
