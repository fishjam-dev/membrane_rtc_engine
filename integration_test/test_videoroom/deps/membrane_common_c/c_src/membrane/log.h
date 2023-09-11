#pragma once
/**
 * Membrane Common C routines: Logging.
 */

#include "_generated/membrane.h"
#include <stdarg.h>

#define MEMBRANE_LOG_LEVEL_DEBUG 0
#define MEMBRANE_LOG_LEVEL_INFO 1
#define MEMBRANE_LOG_LEVEL_WARN 2

#define MEMBRANE_DEFAULT_TAG "Elixir.Membrane.Element.Unknown.Native"

#ifndef MEMBRANE_LOG_TAG
#define MEMBRANE_LOG_TAG MEMBRANE_DEFAULT_TAG
#endif

#define MEMBRANE_DEBUG(env, message, ...)                                      \
  membrane_log(env, MEMBRANE_LOG_LEVEL_DEBUG, MEMBRANE_LOG_TAG, 0, message,    \
               ##__VA_ARGS__)
#define MEMBRANE_INFO(env, message, ...)                                       \
  membrane_log(env, MEMBRANE_LOG_LEVEL_INFO, MEMBRANE_LOG_TAG, 0, message,     \
               ##__VA_ARGS__)
#define MEMBRANE_WARN(env, message, ...)                                       \
  membrane_log(env, MEMBRANE_LOG_LEVEL_WARN, MEMBRANE_LOG_TAG, 0, message,     \
               ##__VA_ARGS__)
#define MEMBRANE_THREADED_DEBUG(env, message, ...)                             \
  membrane_log(env, MEMBRANE_LOG_LEVEL_DEBUG, MEMBRANE_LOG_TAG, 1, message,    \
               ##__VA_ARGS__)
#define MEMBRANE_THREADED_INFO(env, message, ...)                              \
  membrane_log(env, MEMBRANE_LOG_LEVEL_INFO, MEMBRANE_LOG_TAG, 1, message,     \
               ##__VA_ARGS__)
#define MEMBRANE_THREADED_WARN(env, message, ...)                              \
  membrane_log(env, MEMBRANE_LOG_LEVEL_WARN, MEMBRANE_LOG_TAG, 1, message,     \
               ##__VA_ARGS__)

int membrane_log(UnifexEnv *env, int level, char *log_tag, int is_threaded,
                 const char *format, ...);
