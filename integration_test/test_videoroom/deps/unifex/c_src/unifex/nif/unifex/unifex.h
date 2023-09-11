#pragma once

#include <erl_nif.h>
#include <stddef.h>

#define UNIFEX_TERM ERL_NIF_TERM

#define UNIFEX_UNUSED(x) (void)(x)

#define UNIFEX_MAYBE_UNUSED(x) UNIFEX_UNUSED(x)

#define UNIFEX_NO_FLAGS 0

#define UNIFEX_SEND_THREADED 1
#define UNIFEX_FROM_CREATED_THREAD UNIFEX_SEND_THREADED

#ifdef __cplusplus
extern "C" {
#endif

typedef ErlNifEnv UnifexEnv;

typedef ErlNifPid UnifexPid;

static inline void *unifex_alloc(size_t size) { return enif_alloc(size); }

static inline void *unifex_realloc(void *ptr, size_t size) {
  return enif_realloc(ptr, size);
}

static inline void unifex_free(void *ptr) { enif_free(ptr); }

UnifexEnv *unifex_alloc_env(UnifexEnv *env);

static inline void unifex_clear_env(UnifexEnv *env) { enif_clear_env(env); }

static inline void unifex_free_env(UnifexEnv *env) { enif_free_env(env); }

// Mutexes
typedef ErlNifMutex UnifexMutex;
static inline UnifexMutex *unifex_mutex_create(char *name) {
  return enif_mutex_create(name);
}
static inline char *unifex_mutex_name(UnifexMutex *mtx) {
  return enif_mutex_name(mtx);
}
static inline void unifex_mutex_destroy(UnifexMutex *mtx) {
  enif_mutex_destroy(mtx);
}
static inline void unifex_mutex_lock(UnifexMutex *mtx) { enif_mutex_lock(mtx); }
static inline int unifex_mutex_trylock(UnifexMutex *mtx) {
  return enif_mutex_trylock(mtx);
}
static inline void unifex_mutex_unlock(UnifexMutex *mtx) {
  enif_mutex_unlock(mtx);
}

// Condition variables
typedef ErlNifCond UnifexCond;
static inline UnifexCond *unifex_cond_create(char *name) {
  return enif_cond_create(name);
}
static inline char *unifex_cond_name(UnifexCond *cond) {
  return enif_cond_name(cond);
}
static inline void unifex_cond_signal(UnifexCond *cond) {
  enif_cond_signal(cond);
}
static inline void unifex_cond_wait(UnifexCond *cond, UnifexMutex *mutex) {
  enif_cond_wait(cond, mutex);
}
static inline void unifex_cond_broadcast(UnifexCond *cond) {
  enif_cond_broadcast(cond);
}
static inline void unifex_cond_destroy(UnifexCond *cond) {
  enif_cond_destroy(cond);
}

// Threads
typedef ErlNifTid UnifexTid;
static inline int unifex_thread_create(char *name, UnifexTid *tid,
                                       void *(*func)(void *), void *args) {
  return enif_thread_create(name, tid, func, args, NULL);
}
static inline void unifex_thread_exit(void *exit_val) {
  enif_thread_exit(exit_val);
}
static inline int unifex_thread_join(UnifexTid tid, void **exit_val) {
  return enif_thread_join(tid, exit_val);
}

// Time
typedef ErlNifTime UnifexTime;
typedef ErlNifTimeUnit UnifexTimeUnit;
#define UNIFEX_TIME_SEC ERL_NIF_SEC
#define UNIFEX_TIME_MSEC ERL_NIF_MSEC
#define UNIFEX_TIME_USEC ERL_NIF_USEC
#define UNIFEX_TIME_NSEC ERL_NIF_NSEC

static inline UnifexTime unifex_monotonic_time(UnifexTimeUnit unit) {
  return enif_monotonic_time(unit);
}

UNIFEX_TERM unifex_raise(ErlNifEnv *env, const char *description);

// args parse helpers
UNIFEX_TERM unifex_raise_args_error(ErlNifEnv *env, const char *field,
                                    const char *description);

// term manipulation helpers
UNIFEX_TERM unifex_make_resource(ErlNifEnv *env, void *resource);
void unifex_release_resource(void *resource);
int unifex_string_from_term(ErlNifEnv *env, ERL_NIF_TERM input_term,
                            char **string);
UNIFEX_TERM unifex_string_to_term(ErlNifEnv *env, const char *string);
int unifex_alloc_and_get_atom(ErlNifEnv *env, ERL_NIF_TERM atom_term,
                              char **output);
int unifex_parse_bool(ErlNifEnv *env, ERL_NIF_TERM atom_term, int *output);

// send & pid helpers
int unifex_send(UnifexEnv *env, UnifexPid *pid, UNIFEX_TERM term, int flags);
int unifex_get_pid_by_name(UnifexEnv *env, char *name, int flags,
                           UnifexPid *pid);
static inline UnifexPid *unifex_self(UnifexEnv *env, UnifexPid *pid) {
  return enif_self(env, pid);
}

#ifdef __cplusplus
}
#endif
