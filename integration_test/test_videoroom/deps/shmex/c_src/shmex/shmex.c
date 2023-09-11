#define NAME_MAX 255
#define _POSIX_C_SOURCE 200809L

#include <bunch/bunch.h>
#include <erl_nif.h>
#include <fcntl.h>
#include <shmex/shmex.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h> /* For mode constants */
#include <sys/types.h>
#include <unistd.h>

ErlNifResourceType *SHMEX_GUARD_RESOURCE_TYPE;

int load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
  BUNCH_UNUSED(load_info);
  BUNCH_UNUSED(priv_data);

  int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
  SHMEX_GUARD_RESOURCE_TYPE = enif_open_resource_type(
      env, NULL, "ShmexGuard", shmex_guard_destructor, flags, NULL);
  return 0;
}

static ERL_NIF_TERM export_allocate(ErlNifEnv *env, int argc,
                                    const ERL_NIF_TERM argv[]) {
  BUNCH_UNUSED(argc);
  PARSE_SHMEX_ARG(0, payload);
  ERL_NIF_TERM return_term;

  ShmexLibResult result =
      shmex_allocate(env, SHMEX_GUARD_RESOURCE_TYPE, &payload);

  if (SHMEX_RES_OK == result) {
    return_term = bunch_make_ok_tuple(env, shmex_make_term(env, &payload));
  } else {
    return_term = shmex_make_error_term(env, result);
  }

  shmex_release(&payload);
  return return_term;
}

static ERL_NIF_TERM export_add_guard(ErlNifEnv *env, int argc,
                                     const ERL_NIF_TERM argv[]) {
  BUNCH_UNUSED(argc);
  PARSE_SHMEX_ARG(0, payload);

  ShmexGuard *guard;
  if (enif_get_resource(env, payload.guard, SHMEX_GUARD_RESOURCE_TYPE,
                        (void **)&guard)) {
    return bunch_make_error(env, enif_make_atom(env, "already_guarded"));
  };
  shmex_add_guard(env, SHMEX_GUARD_RESOURCE_TYPE, &payload);
  return bunch_make_ok_tuple(env, shmex_make_term(env, &payload));
}

static ERL_NIF_TERM export_set_capacity(ErlNifEnv *env, int argc,
                                        const ERL_NIF_TERM argv[]) {
  BUNCH_UNUSED(argc);
  PARSE_SHMEX_ARG(0, payload);
  BUNCH_PARSE_UINT_ARG(1, capacity);
  ERL_NIF_TERM return_term;

  ShmexLibResult result = shmex_set_capacity(&payload, capacity);
  if (SHMEX_RES_OK == result) {
    return_term = bunch_make_ok_tuple(env, shmex_make_term(env, &payload));
  } else {
    return_term = shmex_make_error_term(env, result);
  }
  shmex_release(&payload);
  return return_term;
}

static ERL_NIF_TERM export_read(ErlNifEnv *env, int argc,
                                const ERL_NIF_TERM argv[]) {
  BUNCH_UNUSED(argc);
  PARSE_SHMEX_ARG(0, payload);
  BUNCH_PARSE_UINT_ARG(1, cnt);

  ERL_NIF_TERM return_term;
  if (cnt > payload.size) {
    return_term = bunch_make_error_str(env, "invalid_read_size");
    goto exit_read;
  }

  ShmexLibResult result = shmex_open_and_mmap(&payload);
  if (SHMEX_RES_OK != result) {
    return_term = shmex_make_error_term(env, result);
    goto exit_read;
  }

  ERL_NIF_TERM out_bin_term;
  unsigned char *output_data = enif_make_new_binary(env, cnt, &out_bin_term);
  memcpy(output_data, payload.mapped_memory, cnt);

  return_term = bunch_make_ok_tuple(env, out_bin_term);
exit_read:
  shmex_release(&payload);
  return return_term;
}

static ERL_NIF_TERM export_write(ErlNifEnv *env, int argc,
                                 const ERL_NIF_TERM argv[]) {
  BUNCH_UNUSED(argc);
  PARSE_SHMEX_ARG(0, payload);
  BUNCH_PARSE_BINARY_ARG(1, data);
  ERL_NIF_TERM return_term;

  if (payload.capacity < data.size) {
    shmex_set_capacity(&payload, data.size);
  }

  ShmexLibResult result = shmex_open_and_mmap(&payload);
  if (SHMEX_RES_OK != result) {
    return_term = shmex_make_error_term(env, result);
    goto exit_write;
  }

  memcpy(payload.mapped_memory, (void *)data.data, data.size);
  payload.size = data.size;
  return_term = bunch_make_ok_tuple(env, shmex_make_term(env, &payload));
exit_write:
  shmex_release(&payload);
  return return_term;
}

static ERL_NIF_TERM export_split_at(ErlNifEnv *env, int argc,
                                    const ERL_NIF_TERM argv[]) {
  BUNCH_UNUSED(argc);
  PARSE_SHMEX_ARG(0, old_payload);
  BUNCH_PARSE_UINT_ARG(1, split_pos);
  Shmex new_payload;
  shmex_init(env, &new_payload, 4096);

  ERL_NIF_TERM return_term;

  ShmexLibResult result = shmex_open_and_mmap(&old_payload);
  if (SHMEX_RES_OK != result) {
    return_term = shmex_make_error_term(env, result);
    goto exit_split_at;
  }

  int new_size = old_payload.size - split_pos;
  new_payload.capacity = new_size;
  new_payload.size = new_size;

  result = shmex_allocate(env, SHMEX_GUARD_RESOURCE_TYPE, &new_payload);
  if (SHMEX_RES_OK != result) {
    return_term = shmex_make_error_term(env, result);
    goto exit_split_at;
  }

  result = shmex_open_and_mmap(&new_payload);
  if (SHMEX_RES_OK != result) {
    return_term = shmex_make_error_term(env, result);
    goto exit_split_at;
  }

  memcpy(new_payload.mapped_memory, old_payload.mapped_memory + split_pos,
         new_size);

  old_payload.size = split_pos;

  return_term = bunch_make_ok_tuple(
      env, enif_make_tuple2(env, shmex_make_term(env, &old_payload),
                            shmex_make_term(env, &new_payload)));

exit_split_at:
  shmex_release(&old_payload);
  shmex_release(&new_payload);
  return return_term;
}

static ERL_NIF_TERM export_trim_leading(ErlNifEnv *env, int argc,
                                        const ERL_NIF_TERM argv[]) {
  BUNCH_UNUSED(argc);
  PARSE_SHMEX_ARG(0, payload);
  BUNCH_PARSE_UINT_ARG(1, offset);
  ERL_NIF_TERM return_term;
  ShmexLibResult result;

  result = shmex_open_and_mmap(&payload);
  if (SHMEX_RES_OK != result) {
    return_term = shmex_make_error_term(env, result);
    goto exit_trim_leading;
  }

  size_t new_size = payload.size - offset;
  memmove(payload.mapped_memory, payload.mapped_memory + offset, new_size);
  payload.size = new_size;
  return_term = bunch_make_ok_tuple(env, shmex_make_term(env, &payload));
exit_trim_leading:
  shmex_release(&payload);
  return return_term;
}

static ERL_NIF_TERM export_ensure_not_gc(ErlNifEnv *env, int argc,
                                         const ERL_NIF_TERM argv[]) {
  BUNCH_UNUSED(argc);
  PARSE_SHMEX_ARG(0, payload);
  shmex_release(&payload);
  return bunch_make_ok(env);
}

static ERL_NIF_TERM export_append(ErlNifEnv *env, int argc,
                                  const ERL_NIF_TERM argv[]) {
  BUNCH_UNUSED(argc);
  PARSE_SHMEX_ARG(0, left);
  PARSE_SHMEX_ARG(1, right);
  ERL_NIF_TERM return_term;
  ShmexLibResult result;

  size_t new_capacity = left.size + right.size;
  result = shmex_set_capacity(&left, new_capacity);
  if (SHMEX_RES_OK != result) {
    return_term = shmex_make_error_term(env, result);
    goto exit_append;
  }

  result = shmex_open_and_mmap(&left);
  if (SHMEX_RES_OK != result) {
    return_term = shmex_make_error_term(env, result);
    goto exit_append;
  }

  result = shmex_open_and_mmap(&right);
  if (SHMEX_RES_OK != result) {
    return_term = shmex_make_error_term(env, result);
    goto exit_append;
  }

  memcpy(left.mapped_memory + left.size, right.mapped_memory, right.size);
  left.size = new_capacity;
  return_term = bunch_make_ok_tuple(env, shmex_make_term(env, &left));
exit_append:
  shmex_release(&left);
  shmex_release(&right);
  return return_term;
}

static ErlNifFunc nif_funcs[] = {{"allocate", 1, export_allocate, 0},
                                 {"add_guard", 1, export_add_guard, 0},
                                 {"set_capacity", 2, export_set_capacity, 0},
                                 {"read", 2, export_read, 0},
                                 {"write", 2, export_write, 0},
                                 {"split_at", 2, export_split_at, 0},
                                 {"append", 2, export_append, 0},
                                 {"trim_leading", 2, export_trim_leading, 0},
                                 {"ensure_not_gc", 1, export_ensure_not_gc, 0}};

ERL_NIF_INIT(Elixir.Shmex.Native.Nif, nif_funcs, load, NULL, NULL, NULL)
