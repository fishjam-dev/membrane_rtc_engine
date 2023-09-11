// feature test macro for clock_gettime and ftruncate
#define _POSIX_C_SOURCE 200809L

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>

#include "lib.h"

#ifdef SHMEX_NIF
#define ALLOC(X) enif_alloc(X)
#define FREE(X) enif_free(X)
#else
#define ALLOC(X) malloc(X)
#define FREE(X) free(X)
#endif

void shmex_generate_shm_name(char *name, int attempt) {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  snprintf(name, SHMEX_SHM_NAME_LEN, SHMEX_SHM_NAME_PREFIX "%.11ld%.9ld#%.3d",
           ts.tv_sec, ts.tv_nsec, attempt);
}

/**
 * Allocates POSIX shared memory given the data (name, capacity) in Shmex
 * struct.
 *
 * If name in Shmex is set to NULL, the name will be (re)generated until
 * the one that haven't been used is found (at most SHMEX_ALLOC_MAX_ATTEMPTS
 * times).
 *
 * Shared memory can be accessed by using 'shmex_open_and_mmap'.
 * Memory will be unmapped when Shmex struct is freed (by 'shmex_release')
 */
ShmexLibResult shmex_allocate_unguarded(Shmex *payload) {
  ShmexLibResult result;
  int fd = -1;

  static const int open_flags = O_RDWR | O_CREAT | O_EXCL;
  static const int open_privileges = 0666;
  if (payload->name != NULL) {
    fd = shm_open(payload->name, open_flags, open_privileges);
  } else {
    payload->name = ALLOC(SHMEX_SHM_NAME_LEN);
    int attempt = 0;
    do {
      shmex_generate_shm_name(payload->name, attempt);
      fd = shm_open(payload->name, open_flags, open_privileges);
      attempt++;
    } while (fd < 0 && (errno == EEXIST || errno == EAGAIN) &&
             attempt < SHMEX_ALLOC_MAX_ATTEMPTS);
  }
  if (fd < 0) {
    result = SHMEX_ERROR_SHM_OPEN;
    goto shmex_create_exit;
  }

  int ftr_res = ftruncate(fd, payload->capacity);
  if (ftr_res < 0) {
    result = SHMEX_ERROR_FTRUNCATE;
    goto shmex_create_exit;
  }

  result = SHMEX_RES_OK;
shmex_create_exit:
  if (fd > 0) {
    close(fd);
  }
  if (SHMEX_RES_OK != result) {
    if (fd > 0) {
      shm_unlink(payload->name);
    }
  }
  return result;
}

/**
 * Maps shared memory into address space of current process (using mmap)
 *
 * On success sets payload->mapped_memory to a valid pointer. On failure it is
 * set to MAP_FAILED ((void *)-1) and returned result indicates which function
 * failed.
 *
 * Mapped memory has to be released with either 'shmex_release' or
 * 'shmex_unmap'.
 *
 * While memory is mapped the capacity of shm must not be modified.
 */
ShmexLibResult shmex_open_and_mmap(Shmex *payload) {
  ShmexLibResult result;
  int fd = -1;

  fd = shm_open(payload->name, O_RDWR, 0666);
  if (fd < 0) {
    result = SHMEX_ERROR_SHM_OPEN;
    goto shmex_open_and_mmap_exit;
  }

  payload->mapped_memory =
      mmap(NULL, payload->capacity, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
  if (MAP_FAILED == payload->mapped_memory) {
    result = SHMEX_ERROR_MMAP;
    goto shmex_open_and_mmap_exit;
  }

  result = SHMEX_RES_OK;
shmex_open_and_mmap_exit:
  if (fd > 0) {
    close(fd);
  }
  return result;
}

void shmex_unmap(Shmex *payload) {
  if (payload->mapped_memory != MAP_FAILED) {
    munmap(payload->mapped_memory, payload->capacity);
  }
  payload->mapped_memory = MAP_FAILED;
}

/**
 * Sets the capacity of shared memory payload. The struct is updated
 * accordingly.
 *
 * Should not be invoked when shm is mapped into the memory.
 */
ShmexLibResult shmex_set_capacity(Shmex *payload, size_t capacity) {
  ShmexLibResult result;
  int fd = -1;

  if (payload->mapped_memory != MAP_FAILED) {
    result = SHMEX_ERROR_SHM_MAPPED;
    goto shmex_set_capacity_exit;
  }

  fd = shm_open(payload->name, O_RDWR, 0666);
  if (fd < 0) {
    result = SHMEX_ERROR_SHM_OPEN;
    goto shmex_set_capacity_exit;
  }

  int res = ftruncate(fd, capacity);
  if (res < 0) {
    result = SHMEX_ERROR_FTRUNCATE;
    goto shmex_set_capacity_exit;
  }
  payload->capacity = capacity;
  if (payload->size > capacity) {
    // data was discarded with ftruncate, update size
    payload->size = capacity;
  }
  result = SHMEX_RES_OK;
shmex_set_capacity_exit:
  if (fd > 0) {
    close(fd);
  }
  return result;
}

/**
 * Unlinks shared memory segment. Unlinked segment cannot be mapped again and is
 * freed once all its memory mappings are removed (e.g. via `shmex_release`
 * function). This function has to be called **before** `shmex_release`.
 */
ShmexLibResult shmex_unlink(Shmex *payload) {
  if (payload->name != NULL) {
    shmex_shm_unlink(payload->name);
    return SHMEX_RES_OK;
  } else {
    return SHMEX_ERROR_INVALID_PAYLOAD;
  }
}

/**
 * Unlinks shared memory segment by name. Works the same way as `shm_unlink`,
 * but contains checks to prevent name conflicts when dealing with SHMs
 * allocated with `shmex_allocate`.
 */
void shmex_shm_unlink(char *name) {
  static const unsigned name_cmp_prefix_len =
      SHMEX_SHM_NAME_PREFIX_LEN + SHMEX_SHM_NAME_TIME_ID_LEN;
  char current_name[SHMEX_SHM_NAME_LEN];
  if (!strncmp(name, SHMEX_SHM_NAME_PREFIX, SHMEX_SHM_NAME_PREFIX_LEN)) {
    // The busy wait below applies only to SHMs with names generated by Shmex
    // and guarantees that such names will not be reused. Although this is a
    // very rare case, it may lead to RC in the following scenario:
    //
    // * SHM is allocated
    // * then it is unliked immediately
    // * another SHM is allocated with the same name
    // * some other descructor tries to unlink the first SHM, but in fact
    //   it unlinks the second one, because it has the same name
    // * the new SHM is no longer accessible
    //
    // Because Shmex assigns names basing on system monotonic time, it is
    // sufficient to wait here for a moment (usually one microsecond or
    // nanosecond, depending on OS clock resolution) to make sure that names
    // will not be reused.
    do {
      shmex_generate_shm_name(current_name, 0);
    } while (strncmp(name, current_name, name_cmp_prefix_len) >= 0);
  }
  shm_unlink(name);
}

const char *shmex_lib_result_to_string(ShmexLibResult result) {
  switch (result) {
  case SHMEX_RES_OK:
    return "ok";
  case SHMEX_ERROR_SHM_OPEN:
    return "shm_open";
  case SHMEX_ERROR_FTRUNCATE:
    return "ftruncate";
  case SHMEX_ERROR_MMAP:
    return "mmap";
  case SHMEX_ERROR_SHM_MAPPED:
    return "shm_is_mapped";
  case SHMEX_ERROR_INVALID_PAYLOAD:
    return "invalid_payload";
  default:
    return "unknown";
  }
}
