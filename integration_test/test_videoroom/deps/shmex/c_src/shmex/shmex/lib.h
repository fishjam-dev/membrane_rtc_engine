#pragma once

#include <math.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#ifdef SHMEX_NIF
#include <erl_nif.h>
#endif
#ifdef SHMEX_CNODE
#include <ei.h>
#endif

#define SHMEX_ELIXIR_STRUCT_ENTRIES 5
#define SHMEX_SHM_NAME_PREFIX "/shmex-"
#define SHMEX_ALLOC_MAX_ATTEMPTS 1000
#define SHMEX_SHM_NAME_PREFIX_LEN strlen(SHMEX_SHM_NAME_PREFIX)
#define SHMEX_SHM_NAME_TIME_ID_LEN 20
#define SHMEX_SHM_NAME_LEN                                                     \
  (SHMEX_SHM_NAME_PREFIX_LEN + SHMEX_SHM_NAME_TIME_ID_LEN +                    \
   (1 + (int)ceil(log10(SHMEX_ALLOC_MAX_ATTEMPTS))) + 1)
#define SHMEX_ELIXIR_STRUCT_ATOM "Elixir.Shmex"

typedef struct {
  char *name;
  unsigned int size;
  unsigned int capacity;
  void *mapped_memory;
#ifdef SHMEX_NIF
  ERL_NIF_TERM guard;
#endif
#ifdef SHMEX_CNODE
  erlang_ref *guard;
#endif
} Shmex;

typedef enum ShmexLibResult {
  SHMEX_RES_OK,
  SHMEX_ERROR_SHM_OPEN,
  SHMEX_ERROR_FTRUNCATE,
  SHMEX_ERROR_MMAP,
  SHMEX_ERROR_SHM_MAPPED,
  SHMEX_ERROR_INVALID_PAYLOAD
} ShmexLibResult;

void shmex_generate_shm_name(char *name, int attempt);
ShmexLibResult shmex_allocate_unguarded(Shmex *payload);
ShmexLibResult shmex_open_and_mmap(Shmex *payload);
ShmexLibResult shmex_set_capacity(Shmex *payload, size_t capacity);
void shmex_unmap(Shmex *payload);
ShmexLibResult shmex_unlink(Shmex *payload);
const char *shmex_lib_result_to_string(ShmexLibResult result);
void shmex_shm_unlink(char *name);
