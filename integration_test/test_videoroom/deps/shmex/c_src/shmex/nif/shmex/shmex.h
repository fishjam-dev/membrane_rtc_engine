#pragma once

#define SHMEX_NIF
#include <bunch/bunch.h>
#include <erl_nif.h>
#include <shmex/lib.h>

#define NAME_MAX 255

typedef struct _ShmexGuard {
  char name[NAME_MAX + 1];
} ShmexGuard;

void shmex_init(ErlNifEnv *env, Shmex *payload, unsigned capacity);
ShmexLibResult shmex_allocate(ErlNifEnv *env, ErlNifResourceType *guard_type,
                              Shmex *payload);
void shmex_add_guard(ErlNifEnv *env, ErlNifResourceType *guard_type,
                     Shmex *payload);
void shmex_guard_destructor(ErlNifEnv *env, void *resource);
int shmex_get_from_term(ErlNifEnv *env, ERL_NIF_TERM record, Shmex *payload);
void shmex_release(Shmex *payload);
ERL_NIF_TERM shmex_make_term(ErlNifEnv *env, Shmex *payload);
ERL_NIF_TERM shmex_make_error_term(ErlNifEnv *env, ShmexLibResult result);

#define PARSE_SHMEX_ARG(position, var_name)                                    \
  BUNCH_PARSE_ARG(position, var_name, Shmex var_name, shmex_get_from_term,     \
                  &var_name)
