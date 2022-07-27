/*
 * This file is part of the Yices SMT Solver.
 * Copyright (C) 2017 SRI International.
 *
 * Yices is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Yices is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Yices.  If not, see <http://www.gnu.org/licenses/>.
 */

/*
 * USE VECTOR = VECTOR OF POINTERS TO TERMS OR OTHER OBJECTS
 */

#include <assert.h>

#include "utils/memalloc.h"
#include "utils/use_vectors.h"


/*
 * Initialize vector v: n = initial size
 * - if n == 0, don't allocate anything
 */
void init_use_vector(use_vector_t *v, uint32_t n) {
  if (n >= MAX_USE_VECTOR_SIZE) {
    out_of_memory();
  }

  v->size = n;
  v->last = 0;
  v->nelems = 0;
  v->free = -1;
  v->data = NULL;
  if (n > 0) {
    v->data = (void **) safe_malloc(n * sizeof(void *));
  }
}


/*
 * Resize: make size large enough for at least n elements
 */
void resize_use_vector(use_vector_t *v, uint32_t n) {
  if (v->size < n) {
    if (n >= MAX_USE_VECTOR_SIZE) {
      out_of_memory();
    }
    v->data = (void **) safe_realloc(v->data, n * sizeof(void *));
    v->size = n;
  }
}


/*
 * Extend: increase size by 50% or make it at least DEFAULT_USE_VECTOR_SIZE
 */
static void extend_use_vector(use_vector_t *v) {
  uint32_t n;

  n = v->size + 1;
  n += n>>1;
  if (n < DEFAULT_USE_VECTOR_SIZE) {
    n = DEFAULT_USE_VECTOR_SIZE;
  }

  if (n >= MAX_USE_VECTOR_SIZE) {
    out_of_memory();
  }

  v->data = (void **) safe_realloc(v->data, n * sizeof(void *));
  v->size = n;
}



/*
 * Return the index of an empty entry
 * - increase size if the vector is full
 */
int32_t alloc_use_vector_entry(use_vector_t *v) {
  int32_t i;

  i = v->free;
  if (i >= 0) {
    assert(empty_entry(v->data[i]));
    v->free = entry2index(v->data[i]);
  } else {
    assert(v->last <= v->size);
    i = v->last;
    v->last = i + 1;
    if (i == v->size) {
      extend_use_vector(v);
    }
  }

  return i;
}



/*
 * Delete v
 */
void delete_use_vector(use_vector_t *v) {
  safe_free(v->data);
  v->data = NULL;
}
