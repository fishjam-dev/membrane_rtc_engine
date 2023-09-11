#include "cnode.h"
#if defined(__FreeBSD__)
#include <sys/socket.h>
#include <netinet/in.h>
#endif
#include <arpa/inet.h>
#include <unistd.h>

#ifdef UNIFEX_CNODE_DEBUG
#define DEBUG(X, ...) fprintf(stderr, X "\r\n", ##__VA_ARGS__)
#else
#define DEBUG(...)
#endif

void unifex_cnode_prepare_ei_x_buff(UnifexEnv *env, ei_x_buff *buff,
                                    const char *msg_type) {
  ei_x_new_with_version(buff);
  ei_x_encode_tuple_header(buff, 2);
  ei_x_encode_atom(buff, env->node_name);
  ei_x_encode_tuple_header(buff, 2);
  ei_x_encode_atom(buff, msg_type);
}

void unifex_cnode_send_and_free(UnifexEnv *env, erlang_pid *pid,
                                UNIFEX_TERM out_buff) {
  ei_send(env->ei_socket_fd, pid, out_buff->buff, out_buff->index);
  ei_x_free(out_buff);
  free(out_buff);
}

void unifex_cnode_reply_and_free(UnifexEnv *env, UNIFEX_TERM out_buff) {
  ei_x_buff *buff;
  if (env->error) {
    buff = env->error;
    env->error = NULL;
  } else {
    buff = out_buff;
  }
  unifex_cnode_send_and_free(env, env->reply_to, buff);
}

UNIFEX_TERM unifex_cnode_undefined_function_error(UnifexEnv *env,
                                                  const char *fun_name) {
  ei_x_buff *out_buff = malloc(sizeof(ei_x_buff));
  unifex_cnode_prepare_ei_x_buff(env, out_buff, "error");
  ei_x_encode_tuple_header(out_buff, 2);
  ei_x_encode_atom(out_buff, "undefined_function");
  ei_x_encode_atom(out_buff, fun_name);
  return out_buff;
}

void unifex_cnode_add_to_released_states(UnifexEnv *env, void *state) {
  UnifexLinkedList *list = malloc(sizeof(UnifexLinkedList));
  list->head = state;
  list->tail = env->released_states;
  env->released_states = list;
}

ei_x_buff unifex_cnode_string_to_list(UnifexCNodeInBuff *origin_buff,
                                      unsigned int strlen) {
  char *str = malloc(sizeof(char) * strlen);
  ei_decode_string(origin_buff->buff, origin_buff->index, str);
  unsigned char *unsigned_str = (unsigned char *)str;

  ei_x_buff buff;
  ei_x_new(&buff);
  ei_x_encode_list_header(&buff, strlen);
  for (unsigned int i = 0; i < strlen; i++) {
    ei_x_encode_longlong(&buff, (long long)unsigned_str[i]);
  }
  ei_x_encode_empty_list(&buff);

  free(str);
  return buff;
}

static void free_released_states(UnifexEnv *env) {
  while (env->released_states) {
    void *state = env->released_states->head;
    if (state != env->state) {
      unifex_cnode_destroy_state(env, state);
    }
    UnifexLinkedList *tail = env->released_states->tail;
    free(env->released_states);
    env->released_states = tail;
  }
}

int unifex_cnode_receive(UnifexEnv *env) {
  ei_x_buff in_buff;
  ei_x_new(&in_buff);
  erlang_msg emsg;
  int res = 0;
  switch (ei_xreceive_msg_tmo(env->ei_socket_fd, &emsg, &in_buff, 100)) {
  case ERL_TICK:
    break;
  case ERL_ERROR:
    res = erl_errno != ETIMEDOUT;
    break;
  default:
    if (emsg.msgtype == ERL_REG_SEND) {
      env->reply_to = &emsg.from;
      int index = 0;
      int version;
      ei_decode_version(in_buff.buff, &index, &version);

      int arity;
      ei_decode_tuple_header(in_buff.buff, &index, &arity);

      char fun_name[2048];
      ei_decode_atom(in_buff.buff, &index, fun_name);

      UnifexCNodeInBuff buff = {.buff = in_buff.buff, .index = &index};
      UNIFEX_TERM result = unifex_cnode_handle_message(env, fun_name, &buff);
      unifex_cnode_reply_and_free(env, result);
      free_released_states(env);
      break;
    }
  }
  ei_x_free(&in_buff);
  return res;
}

static int validate_args(int argc, char **argv) {
  if (argc != 5) {
    return 1;
  }
  for (int i = 1; i < argc; i++) {
    if (strlen(argv[i]) > 255) {
      return 1;
    }
  }
  return 0;
}

static int listen_sock(int *listen_fd, int *port) {
  int fd = socket(AF_INET, SOCK_STREAM, 0);
  if (fd < 0) {
    return 1;
  }

  int opt_on = 1;
  if (setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &opt_on, sizeof(opt_on))) {
    return 1;
  }

  struct sockaddr_in addr;
  unsigned int addr_size = sizeof(addr);
  addr.sin_family = AF_INET;
  addr.sin_port = htons(0);
  addr.sin_addr.s_addr = htonl(INADDR_ANY);

  if (bind(fd, (struct sockaddr *)&addr, addr_size) < 0) {
    return 1;
  }

  if (getsockname(fd, (struct sockaddr *)&addr, &addr_size)) {
    return 1;
  }
  *port = (int)ntohs(addr.sin_port);

  const int queue_size = 5;
  if (listen(fd, queue_size)) {
    return 1;
  }

  *listen_fd = fd;
  return 0;
}

int unifex_cnode_init(int argc, char **argv, UnifexEnv *env) {
  *env = (UnifexEnv){.node_name = calloc(sizeof(char), 256),
                     .ei_socket_fd = ERL_ERROR,
                     .listen_fd = -1,
                     .reply_to = NULL,
                     .state = NULL,
                     .released_states = NULL,
                     .error = NULL};

  if (validate_args(argc, argv)) {
    fprintf(stderr,
            "%s <host_name> <alive_name> <node_name> <cookie> <creation>\r\n",
            argv[0]);
    goto unifex_cnode_init_error;
  }
  char host_name[256];
  strcpy(host_name, argv[1]);
  char alive_name[256];
  strcpy(alive_name, argv[2]);
  strcpy(env->node_name, argv[3]);
  short creation = (short)atoi(argv[4]);
  char *cookie = getenv("BUNDLEX_ERLANG_COOKIE");

  int port;
  if (listen_sock(&env->listen_fd, &port)) {
    DEBUG("listen error");
    goto unifex_cnode_init_error;
  }
  DEBUG("listening at %d", port);

  ei_cnode ec;
  struct in_addr addr;
  addr.s_addr = inet_addr("127.0.0.1");
  if (ei_connect_xinit(&ec, host_name, alive_name, env->node_name, &addr,
                       cookie, creation) < 0) {
    DEBUG("init error: %d", erl_errno);
    goto unifex_cnode_init_error;
  }
  DEBUG("initialized %s (%s)", ei_thisnodename(&ec), inet_ntoa(addr));

  if (ei_publish(&ec, port) == -1) {
    DEBUG("publish error: %d", erl_errno);
    goto unifex_cnode_init_error;
  }
  DEBUG("published");
  printf("ready\r\n");
  fflush(stdout);

  ErlConnect conn;
  env->ei_socket_fd = ei_accept_tmo(&ec, env->listen_fd, &conn, 5000);
  if (env->ei_socket_fd == ERL_ERROR) {
    DEBUG("accept error: %d", erl_errno);
    goto unifex_cnode_init_error;
  }
  DEBUG("accepted %s", conn.nodename);

  return 0;

unifex_cnode_init_error:
  unifex_cnode_destroy(env);
  return 1;
}

void unifex_cnode_destroy(UnifexEnv *env) {
  if (env->listen_fd != -1) {
    close(env->listen_fd);
  }
  if (env->ei_socket_fd != ERL_ERROR) {
    close(env->ei_socket_fd);
  }
  if (env->node_name) {
    free(env->node_name);
  }
}

int unifex_cnode_main_function(int argc, char **argv) {
  UnifexEnv env;
  if (unifex_cnode_init(argc, argv, &env)) {
    return 1;
  }

  while (!unifex_cnode_receive(&env))
    ;

  unifex_cnode_destroy(&env);
  return 0;
}
