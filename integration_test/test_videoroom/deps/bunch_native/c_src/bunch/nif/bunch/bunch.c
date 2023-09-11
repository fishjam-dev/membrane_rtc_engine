#include "bunch.h"

/**
 * Builds `:ok`.
 */
ERL_NIF_TERM bunch_make_ok(ErlNifEnv *env) { return enif_make_atom(env, "ok"); }

/**
 * Builds `{:ok, arg}`.
 */
ERL_NIF_TERM bunch_make_ok_tuple(ErlNifEnv *env, ERL_NIF_TERM arg) {
  return enif_make_tuple2(env, bunch_make_ok(env), arg);
}

/**
 * Builds `{:error, reason}`.
 */
ERL_NIF_TERM bunch_make_error(ErlNifEnv *env, ERL_NIF_TERM reason) {
  return enif_make_tuple2(env, enif_make_atom(env, "error"), reason);
}

/**
 * Builds `{:error, reason_atom}` where `reason_atom` is created out of
 * `reason`.
 */
ERL_NIF_TERM bunch_make_error_str(ErlNifEnv *env, const char *reason) {
  return enif_make_tuple2(env, enif_make_atom(env, "error"),
                          enif_make_atom(env, reason));
}

/**
 * Helper for returning errors from system calls using errno.
 * Builds `{:error, {errno_atom, fun_called_atom}}` where `errno_atom`
 * is lowercase version of POSIX error name and `fun_called_atom` is the name
 * of the function that returned error.
 */
ERL_NIF_TERM bunch_make_error_errno(ErlNifEnv *env, const char *call) {
  ERL_NIF_TERM tuple =
      enif_make_tuple2(env, enif_make_atom(env, bunch_errno_string()),
                       enif_make_atom(env, call));

  return bunch_make_error(env, tuple);
}

/**
 * Builds error description for invalid argument error and raises using
 * `bunch_raise_error`.
 */
ERL_NIF_TERM bunch_raise_error_args(ErlNifEnv *env, const char *field,
                                    const char *reason) {
  char *description = enif_alloc(strlen(field) + strlen(reason) + 255);
  sprintf(description, "Invalid argument %s, reason: %s", field, reason);
  ERL_NIF_TERM result = bunch_raise_error(env, description);
  enif_free(description);
  return result;
}

/**
 * Causes raising error with descripton `description` when returning from NIF.
 *
 * See http://erlang.org/doc/man/erl_nif.html#enif_raise_exception
 */
ERL_NIF_TERM bunch_raise_error(ErlNifEnv *env, const char *description) {
  return enif_raise_exception(
      env, enif_make_string(env, description, ERL_NIF_LATIN1));
}
