#pragma once

#include <bunch/lib.h>
#include <erl_nif.h>
#include <stdarg.h>
#include <string.h>

// varargs parse helpers
#define BUNCH_PARSE_ARG(position, var_name, var_def, getter_func, ...)         \
  var_def;                                                                     \
  if (!getter_func(env, argv[position], __VA_ARGS__)) {                        \
    return bunch_raise_error_args(env, #var_name, #getter_func);               \
  }

#define BUNCH_PARSE_UINT_ARG(position, var_name)                               \
  BUNCH_PARSE_ARG(position, var_name, unsigned int var_name, enif_get_uint,    \
                  &var_name)

#define BUNCH_PARSE_INT_ARG(position, var_name)                                \
  BUNCH_PARSE_ARG(position, var_name, int var_name, enif_get_int, &var_name)

#define BUNCH_PARSE_LONG_ARG(position, var_name)                               \
  BUNCH_PARSE_ARG(position, var_name, long var_name, enif_get_long, &var_name)

#define BUNCH_PARSE_ATOM_ARG(position, var_name, max_size)                     \
  BUNCH_PARSE_ARG(position, var_name, char var_name[max_size], enif_get_atom,  \
                  (char *)var_name, max_size, ERL_NIF_LATIN1)

#define BUNCH_PARSE_STRING_ARG(position, var_name, max_size)                   \
  BUNCH_PARSE_ARG(position, var_name, char var_name[max_size],                 \
                  enif_get_string, (char *)var_name, max_size, ERL_NIF_LATIN1)

#define BUNCH_PARSE_BINARY_ARG(position, var_name)                             \
  BUNCH_PARSE_ARG(position, var_name, ErlNifBinary var_name,                   \
                  enif_inspect_binary, &var_name)

#define BUNCH_PARSE_RESOURCE_ARG(position, var_name, var_type, res_type)       \
  var_type *var_name;                                                          \
  if (!enif_get_resource(env, argv[position], res_type, (void **)&var_name)) { \
    return bunch_raise_error_args(env, #var_name, "enif_get_resource");        \
  }

#define BUNCH_PARSE_PID_ARG(position, var_name)                                \
  BUNCH_PARSE_ARG(position, var_name, ErlNifPid var_name, enif_get_local_pid,  \
                  &var_name)

ERL_NIF_TERM bunch_make_ok(ErlNifEnv *env);
ERL_NIF_TERM bunch_make_ok_tuple(ErlNifEnv *env, ERL_NIF_TERM arg);
ERL_NIF_TERM bunch_make_error(ErlNifEnv *env, ERL_NIF_TERM reason);
ERL_NIF_TERM bunch_make_error_str(ErlNifEnv *env, const char *reason);
ERL_NIF_TERM bunch_make_error_errno(ErlNifEnv *env, const char *call);
ERL_NIF_TERM bunch_raise_error_args(ErlNifEnv *env, const char *field,
                                    const char *reason);
ERL_NIF_TERM bunch_raise_error(ErlNifEnv *env, const char *description);
