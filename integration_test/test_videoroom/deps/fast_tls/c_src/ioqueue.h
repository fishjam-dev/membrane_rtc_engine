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

#ifndef IOQUEUE_H
#define IOQUEUE_H

#include <stddef.h>

typedef struct ioqueue {
    char *buf;
    size_t size;
    size_t capacity;
} ioqueue;

ioqueue *ioqueue_create();
void ioqueue_free(ioqueue *queue);
void ioqueue_consume(ioqueue *queue, size_t bytes);
int ioqueue_append(ioqueue *queue, const char *buf, size_t bytes);

#endif
