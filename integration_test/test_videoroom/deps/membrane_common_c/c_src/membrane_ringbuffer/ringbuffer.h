#pragma once
/**
 * Membrane Common C routines: RingBuffer.
 */

#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unifex/unifex.h>

typedef struct _MembraneRingBuffer MembraneRingBuffer;
struct _MembraneRingBuffer {
  void *buffer;
  size_t element_size;
  size_t max_elements;
  atomic_size_t write_index;
  atomic_size_t read_index;
};

MembraneRingBuffer *membrane_ringbuffer_new(size_t element_count,
                                            size_t element_size);
size_t membrane_ringbuffer_write(MembraneRingBuffer *ringbuffer, void *src,
                                 size_t cnt);
size_t membrane_ringbuffer_get_write_available(MembraneRingBuffer *ringbuffer);
size_t membrane_ringbuffer_get_read_available(MembraneRingBuffer *ringbuffer);
size_t membrane_ringbuffer_read(MembraneRingBuffer *ringbuffer, void *dest,
                                size_t cnt);
void membrane_ringbuffer_cleanup(MembraneRingBuffer *ringbuffer);
void membrane_ringbuffer_destroy(MembraneRingBuffer *ringbuffer);
