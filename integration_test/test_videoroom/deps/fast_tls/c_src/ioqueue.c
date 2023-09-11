/*
 * Copyright (C) 2002-2021 ProcessOne, SARL. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

#include <stdlib.h>
#include <memory.h>
#include "ioqueue.h"

ioqueue *ioqueue_create() {
    ioqueue *q = malloc(sizeof(ioqueue));
    if (q) {
        q->buf = NULL;
        q->size = 0;
        q->capacity = 0;
    }
    return q;
}

void ioqueue_free(ioqueue *queue) {
    if (queue->buf) {
        free(queue->buf);
    }
    free(queue);
}

void ioqueue_consume(ioqueue *queue, size_t bytes) {
    queue->size -= bytes;
    if (queue->size > 0) {
        memmove(queue->buf, queue->buf + bytes, queue->size);
    } else {
        free(queue->buf);
        queue->buf = NULL;
        queue->capacity = 0;
    }
}

int ioqueue_append(ioqueue *queue, const char *buf, size_t bytes) {
    if (queue->capacity - queue->size < bytes) {
        char *new_buf = realloc(queue->buf, queue->size + bytes);
        if (!new_buf) {
            return 0;
        }
        queue->buf = new_buf;
        queue->capacity = queue->size + bytes;
    }
    memcpy(queue->buf + queue->size, buf, bytes);
    queue->size += bytes;
    return 1;
}
