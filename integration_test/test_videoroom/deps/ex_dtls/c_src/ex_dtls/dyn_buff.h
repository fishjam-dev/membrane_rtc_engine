#pragma once

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct DynBuff DynBuff;
struct DynBuff {
  // all sizes are in bytes
  int size;
  int data_size;
  char *data;
};

DynBuff *dyn_buff_new(int size);
void dyn_buff_free(DynBuff *dyn_buff);
int dyn_buff_insert(DynBuff *dyn_buff, char *data, int size);
