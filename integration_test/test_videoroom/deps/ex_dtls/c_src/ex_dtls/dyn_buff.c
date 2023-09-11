#include "dyn_buff.h"

DynBuff *dyn_buff_new(int size) {
  DynBuff *dyn_buff = (DynBuff *)malloc(sizeof(DynBuff));
  dyn_buff->data = (char *)malloc(size * sizeof(char));
  if (dyn_buff->data == NULL) {
    free(dyn_buff);
    return NULL;
  }
  memset(dyn_buff->data, 0, size);
  dyn_buff->size = size;
  dyn_buff->data_size = 0;
  return dyn_buff;
}

int dyn_buff_insert(DynBuff *dyn_buff, char *data, int size) {
  if (dyn_buff->size - dyn_buff->data_size < size) {
    int new_size = 2 * (dyn_buff->data_size + size);
    char *new_data = (char *)realloc(dyn_buff->data, new_size);
    if (new_data == NULL) {
      dyn_buff_free(dyn_buff);
      return -1;
    }
    dyn_buff->data = new_data;
    dyn_buff->size = new_size;
  }
  memcpy(dyn_buff->data + dyn_buff->data_size, data, size);
  dyn_buff->data_size += size;
  return 0;
}

void dyn_buff_free(DynBuff *dyn_buff) { free(dyn_buff->data); }
