#include "unifex.h"
#include "cnode.h"

UNIFEX_TERM unifex_raise(UnifexEnv *env, const char *message) {
  ei_x_buff *out_buff = (ei_x_buff *)malloc(sizeof(ei_x_buff));
  unifex_cnode_prepare_ei_x_buff(env, out_buff, "raise");
  ei_x_encode_binary(out_buff, message, strlen(message));
  return out_buff;
}