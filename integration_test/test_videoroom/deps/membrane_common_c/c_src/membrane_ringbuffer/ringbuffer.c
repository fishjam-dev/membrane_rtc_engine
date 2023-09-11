/**
 * Membrane Common C routines: RingBuffer.
 */
#include "ringbuffer.h"

/**
 * Initializes new ring buffer of given size.
 *
 * It allows lock-free concurrent operation by one consumer and one producer.
 *
 * It should be freed using membrane_ringbuffer_destroy after usage.
 */
MembraneRingBuffer *membrane_ringbuffer_new(size_t max_elements,
                                            size_t element_size) {
  MembraneRingBuffer *ringbuffer = unifex_alloc(sizeof(MembraneRingBuffer));
  ringbuffer->buffer = unifex_alloc(max_elements * element_size);
  /*
   * write_index and read_index will be kept modulo (max_elements * 2)
   * while actual indices for read/write will be calculated using modulo
   * (max_elements) This will allow to get difference between indices equal to
   * max_elements and distinguish full buffer from empty buffer
   */
  ringbuffer->read_index = 0;
  ringbuffer->write_index = 0;
  ringbuffer->element_size = element_size;
  ringbuffer->max_elements = max_elements;

  return ringbuffer;
}

static inline size_t calc_read_available(MembraneRingBuffer *rb,
                                         size_t read_index,
                                         size_t write_index) {
  size_t modulo = (rb->max_elements * 2);
  // we need to make sure the difference is not negative to avoid unsigned
  // integer underflow
  return (write_index + modulo - read_index) % modulo;
}

/**
 * Writes at most `cnt` elements to the ringbuffer.
 *
 * Returns the actual number of elements copied.
 */
size_t membrane_ringbuffer_write(MembraneRingBuffer *ringbuffer, void *src,
                                 size_t cnt) {
  size_t read_index =
      atomic_load_explicit(&ringbuffer->read_index, memory_order_acquire);
  size_t write_index =
      atomic_load_explicit(&ringbuffer->write_index, memory_order_relaxed);
  size_t available = ringbuffer->max_elements -
                     calc_read_available(ringbuffer, read_index, write_index);
  cnt = cnt > available ? available : cnt;

  size_t index = write_index % ringbuffer->max_elements;
  void *dest = ringbuffer->buffer + (index * ringbuffer->element_size);
  // if the write will not be contiguous
  if (index + cnt > ringbuffer->max_elements) {
    size_t tail_items = ringbuffer->max_elements - index;

    size_t copy_size = tail_items * ringbuffer->element_size;
    memcpy(dest, src, copy_size);

    dest = ringbuffer->buffer;
    src = src + copy_size;
    copy_size = (cnt - tail_items) * ringbuffer->element_size;
    memcpy(dest, src, copy_size);
  } else {
    size_t copy_size = cnt * ringbuffer->element_size;
    memcpy(dest, src, copy_size);
  }

  atomic_store_explicit(&ringbuffer->write_index,
                        (write_index + cnt) % (ringbuffer->max_elements * 2),
                        memory_order_release);
  return cnt;
}

/**
 * Returns the number of ringbuffer's available elements for read.
 */
size_t membrane_ringbuffer_get_read_available(MembraneRingBuffer *ringbuffer) {
  size_t wi =
      atomic_load_explicit(&ringbuffer->write_index, memory_order_relaxed);
  size_t ri =
      atomic_load_explicit(&ringbuffer->read_index, memory_order_relaxed);
  return calc_read_available(ringbuffer, ri, wi);
}

/**
 * Returns the number of ringbuffer's available elements for write.
 */
size_t membrane_ringbuffer_get_write_available(MembraneRingBuffer *ringbuffer) {
  return ringbuffer->max_elements -
         membrane_ringbuffer_get_read_available(ringbuffer);
}

/**
 * Reads at most `cnt` elements from the ringbuffer.
 *
 * Returns the actual number of elements copied.
 */
size_t membrane_ringbuffer_read(MembraneRingBuffer *ringbuffer, void *dest,
                                size_t cnt) {
  size_t write_index =
      atomic_load_explicit(&ringbuffer->write_index, memory_order_acquire);
  size_t read_index =
      atomic_load_explicit(&ringbuffer->read_index, memory_order_relaxed);
  size_t available = calc_read_available(ringbuffer, read_index, write_index);
  cnt = cnt > available ? available : cnt;

  size_t index = read_index % ringbuffer->max_elements;
  void *src = ringbuffer->buffer + (index * ringbuffer->element_size);
  // if the read will not be contiguous
  if (index + cnt > ringbuffer->max_elements) {
    size_t tail_items = ringbuffer->max_elements - index;

    size_t copy_size = tail_items * ringbuffer->element_size;
    memcpy(dest, src, copy_size);

    dest = ringbuffer->buffer;
    src = src + (copy_size * ringbuffer->element_size);
    copy_size = (cnt - tail_items) * ringbuffer->element_size;
    memcpy(dest, src, copy_size);
  } else {
    size_t copy_size = cnt * ringbuffer->element_size;
    memcpy(dest, src, copy_size);
  }

  atomic_store_explicit(&ringbuffer->read_index,
                        (read_index + cnt) % (ringbuffer->max_elements * 2),
                        memory_order_release);
  return cnt;
}

/**
 * Reset the read and write pointers to zero. This is not thread safe.
 */
void membrane_ringbuffer_cleanup(MembraneRingBuffer *ringbuffer) {
  ringbuffer->write_index = 0;
  ringbuffer->read_index = 0;
}

/**
 * Destroys given ring buffer.
 *
 */
void membrane_ringbuffer_destroy(MembraneRingBuffer *ringbuffer) {
  unifex_free(ringbuffer->buffer);
  unifex_free(ringbuffer);
}
