#pragma once

#include "unifex.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct UnifexCNodeInBuff {
  const char *buff;
  int *index;
} UnifexCNodeInBuff;

void unifex_cnode_prepare_ei_x_buff(UnifexEnv *env, ei_x_buff *buff,
                                    const char *msg_type);
void unifex_cnode_send_and_free(UnifexEnv *env, erlang_pid *pid,
                                UNIFEX_TERM out_buff);
void unifex_cnode_reply_and_free(UnifexEnv *env, UNIFEX_TERM out_buff);
UNIFEX_TERM unifex_cnode_undefined_function_error(UnifexEnv *env,
                                                  const char *fun_name);

void unifex_cnode_add_to_released_states(UnifexEnv *env, void *state);

UNIFEX_TERM unifex_cnode_handle_message(UnifexEnv *env, char *fun_name,
                                        UnifexCNodeInBuff *in_buff);

void unifex_cnode_destroy_state(UnifexEnv *env, void *state);

int unifex_cnode_init(int argc, char **argv, UnifexEnv *env);
int unifex_cnode_receive(UnifexEnv *env);
ei_x_buff unifex_cnode_string_to_list(UnifexCNodeInBuff *origin_buff,
                                      unsigned int strlen);
void unifex_cnode_destroy(UnifexEnv *env);

#ifdef __cplusplus
}
#endif