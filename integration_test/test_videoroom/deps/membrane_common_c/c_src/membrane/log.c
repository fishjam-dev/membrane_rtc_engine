/**
 * Membrane Common C routines: Logging.
 */

#include "log.h"

#include <stdio.h>
#include <time.h>

static char *log_level_to_string(int level);
static void current_time_as_string(char time[255]);

/**
 * Builds log message from string containing format and from va_list with
 * parameters. It also attaches :nif tag and sends message to the log router.
 *
 * `env` - the environment of the calling process.
 *    Must be NULL if calling from a created thread.
 * `level` - integer describing log level
 * `format` and `va_args` - describe string that should be send
 *
 * On success, returns true. Otherwise, false is returned.
 */
int membrane_log(UnifexEnv *env, int level, char *log_tag, int is_threaded,
                 const char *format, ...) {
  va_list args;

  // compute the space that is needed to store whole message
  va_start(args, format);
  size_t msg_size = vsnprintf(NULL, 0, format, args);
  va_end(args);

  char *msg = unifex_alloc(msg_size + 3);

  // print message to string
  va_start(args, format);
  vsnprintf(msg, msg_size + 1, format, args);
  va_end(args);

  memcpy(msg + msg_size, "\r\n\0", 3);

  char *level_str = log_level_to_string(level);

  char time_str[255];
  current_time_as_string(time_str);

  int flags = is_threaded ? UNIFEX_FROM_CREATED_THREAD : UNIFEX_NO_FLAGS;
  char *tags[] = {"nif", log_tag};

  UnifexPid router_pid;
  int res = unifex_get_pid_by_name(env, "Elixir.Membrane.Log.Router", flags,
                                   &router_pid) &&
            send_membrane_log(env, router_pid, flags, level_str, msg, time_str,
                              tags, 2);
  unifex_free(msg);

  return res;
}

static char *log_level_to_string(int level) {
  switch (level) {
  case 1:
    return "info";
  case 2:
    return "warn";
  default:
    return "debug";
  }
}

static void current_time_as_string(char time_str[255]) {
  time_t raw_time = time(NULL);
  struct tm *current_time = localtime(&raw_time);
  char format[] = "%Y-%m-%dT%H:%M:%SZ";
  strftime(time_str, 255, format, current_time);
}
