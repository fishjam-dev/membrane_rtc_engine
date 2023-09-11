#define SHMEX_CNODE
#include <ei.h>
#include <shmex/lib.h>

#define NAME_MAX 255

void shmex_init(Shmex *payload, unsigned capacity);
int shmex_deserialize(const char *buf, int *idx, Shmex *payload);
void shmex_release(Shmex *payload);
int shmex_serialize(ei_x_buff *buf, Shmex *payload);
// ERL_NIF_TERM shmex_make_error_term(ErlNifEnv * env, ShmexLibResult result);
