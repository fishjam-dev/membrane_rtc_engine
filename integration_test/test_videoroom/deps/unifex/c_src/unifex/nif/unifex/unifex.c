#include "unifex.h"

#include <string.h>
#include <time.h>

UnifexEnv *unifex_alloc_env(UnifexEnv *_env) {
  UNIFEX_UNUSED(_env);
  return enif_alloc_env();
}

UNIFEX_TERM unifex_raise(ErlNifEnv *env, const char *description) {
  return enif_raise_exception(env, unifex_string_to_term(env, description));
}

UNIFEX_TERM unifex_raise_args_error(ErlNifEnv *env, const char *field,
                                    const char *description) {
  ERL_NIF_TERM exception_content = enif_make_tuple2(
      env, enif_make_atom(env, "unifex_parse_arg"),
      enif_make_tuple2(env, enif_make_atom(env, field),
                       enif_make_string(env, description, ERL_NIF_LATIN1)));
  return enif_raise_exception(env, exception_content);
}

UNIFEX_TERM unifex_make_resource(ErlNifEnv *env, void *resource) {
  ERL_NIF_TERM resource_term = enif_make_resource(env, resource);
  return resource_term;
}

void unifex_release_resource(void *resource) {
  enif_release_resource(resource);
}

int unifex_string_from_term(ErlNifEnv *env, ERL_NIF_TERM input_term,
                            char **string) {
  ErlNifBinary binary;
  int res = enif_inspect_binary(env, input_term, &binary);
  if (res) {
    *string = enif_alloc(binary.size + 1);
    memcpy(*string, binary.data, binary.size);
    (*string)[binary.size] = 0;
  }
  return res;
}

int unifex_alloc_and_get_atom(ErlNifEnv *env, ERL_NIF_TERM atom_term,
                              char **output) {
  unsigned length;
  int res = enif_get_atom_length(env, atom_term, &length, ERL_NIF_LATIN1);
  if (!res) {
    return res;
  }

  *output = enif_alloc(length + 1);
  if (*output == NULL) {
    return 0;
  }

  return enif_get_atom(env, atom_term, *output, length + 1, ERL_NIF_LATIN1);
}

int unifex_parse_bool(ErlNifEnv *env, ERL_NIF_TERM atom_term, int *output) {
  static const int TMP_LEN = 6;
  char boolean_str[TMP_LEN];
  int res = enif_get_atom(env, atom_term, boolean_str, TMP_LEN, ERL_NIF_LATIN1);
  if (!res) {
    return res;
  }

  if (strcmp(boolean_str, "true") == 0) {
    *output = 1;
    return 1;
  }
  if (strcmp(boolean_str, "false") == 0) {
    *output = 0;
    return 1;
  }

  return 0;
}

UNIFEX_TERM unifex_string_to_term(ErlNifEnv *env, const char *string) {
  ERL_NIF_TERM output_term;
  int string_length = strlen(string);
  unsigned char *ptr = enif_make_new_binary(env, string_length, &output_term);
  memcpy(ptr, string, string_length);
  return output_term;
}

int unifex_send(UnifexEnv *env, UnifexPid *pid, UNIFEX_TERM term, int flags) {
  int res;
  if (flags & UNIFEX_SEND_THREADED) {
    res = enif_send(NULL, pid, env, term);
    unifex_clear_env(env);
  } else {
    res = enif_send(env, pid, NULL, term);
  }
  return res;
}

int unifex_get_pid_by_name(UnifexEnv *env, char *name, int flags,
                           UnifexPid *pid) {
  UnifexEnv *looking_env = flags & UNIFEX_FROM_CREATED_THREAD ? NULL : env;

  int res = enif_whereis_pid(looking_env, enif_make_atom(env, name), pid);

  return res;
}
