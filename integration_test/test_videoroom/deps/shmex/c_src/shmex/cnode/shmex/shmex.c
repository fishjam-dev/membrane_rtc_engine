#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>

#include "shmex.h"

static int try_decode_nil(const char *buf, int *idx_ptr, int *nil) {
  int idx = *idx_ptr;
  int type, size;
  char atom[4];
  if (ei_get_type(buf, &idx, &type, &size)) {
    return 1;
  }
  if (type != 'd' || size != 3) {
    *nil = 0;
    return 0;
  }
  if (ei_decode_atom(buf, &idx, atom)) {
    return 1;
  }
  if (!strcmp(atom, "nil")) {
    *nil = 1;
    *idx_ptr = idx;
  } else {
    *nil = 0;
  }
  return 0;
}

/**
 * Initializes Shmex C struct. Should be used before allocating shm from C code.
 *
 * Each call should be paired with `shmex_release` call to deallocate resources.
 */
void shmex_init(Shmex *payload, unsigned capacity) {
  payload->size = 0;
  payload->capacity = capacity;
  payload->mapped_memory = MAP_FAILED;
  payload->name = NULL;
  payload->guard = NULL;
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
  if (payload->guard != NULL) {
    free(payload->guard);
    payload->guard = NULL;
  }
  shmex_unmap(payload);
}

int shmex_serialize(ei_x_buff *buf, Shmex *payload) {
  return ei_x_encode_map_header(buf, SHMEX_ELIXIR_STRUCT_ENTRIES) ||
         ei_x_encode_atom(buf, "name") ||
         (payload->name
              ? ei_x_encode_binary(buf, payload->name, strlen(payload->name))
              : ei_x_encode_atom(buf, "nil")) ||
         ei_x_encode_atom(buf, "guard") ||
         (payload->guard ? ei_x_encode_ref(buf, payload->guard)
                         : ei_x_encode_atom(buf, "nil")) ||
         ei_x_encode_atom(buf, "size") ||
         ei_x_encode_ulong(buf, (unsigned long)payload->size) ||
         ei_x_encode_atom(buf, "capacity") ||
         ei_x_encode_ulong(buf, (unsigned long)payload->capacity) ||
         ei_x_encode_atom(buf, "__struct__") ||
         ei_x_encode_atom(buf, SHMEX_ELIXIR_STRUCT_ATOM);
}

int shmex_deserialize(const char *buf, int *idx, Shmex *payload) {
  int map_size;
  if (ei_decode_map_header(buf, idx, &map_size) ||
      map_size != SHMEX_ELIXIR_STRUCT_ENTRIES) {
    return 0;
  }

  shmex_init(payload, 0);

  unsigned long tmp_size;
  int is_nil;

  for (int i = 0; i < SHMEX_ELIXIR_STRUCT_ENTRIES; i++) {
    char key[NAME_MAX];
    ei_decode_atom(buf, idx, key);
    if (!strcmp(key, "name")) {
      if (try_decode_nil(buf, idx, &is_nil)) {
        goto shmex_deserialize_error;
      }
      if (!is_nil) {
        char name[NAME_MAX];
        long name_len;
        if (ei_decode_binary(buf, idx, name, &name_len)) {
          goto shmex_deserialize_error;
        }
        payload->name = malloc(name_len + 1);
        snprintf(payload->name, name_len + 1, "%s", name);
      }
    } else if (!strcmp(key, "guard")) {
      if (try_decode_nil(buf, idx, &is_nil)) {
        goto shmex_deserialize_error;
      }
      if (!is_nil) {
        payload->guard = malloc(sizeof(erlang_ref));
        if (ei_decode_ref(buf, idx, payload->guard)) {
          goto shmex_deserialize_error;
        }
      }
    } else if (!strcmp(key, "size")) {
      if (ei_decode_ulong(buf, idx, &tmp_size)) {
        goto shmex_deserialize_error;
      }
      payload->size = (int)tmp_size;
    } else if (!strcmp(key, "capacity")) {
      if (ei_decode_ulong(buf, idx, &tmp_size)) {
        goto shmex_deserialize_error;
      }
      payload->capacity = (int)tmp_size;
    } else if (!strcmp(key, "__struct__")) {
      char struct_name[NAME_MAX];
      if (ei_decode_atom(buf, idx, struct_name) ||
          strcmp(struct_name, SHMEX_ELIXIR_STRUCT_ATOM)) {
        goto shmex_deserialize_error;
      }
    } else {
      goto shmex_deserialize_error;
    }
  }

  return 0;

shmex_deserialize_error:
  shmex_release(payload);
  return 1;
}
