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
 * HASH SETS THAT STORE PAIRS OF NON-NEGATIVE INTEGERS
 */

#ifndef __PAIR_HASH_SETS_H
#define __PAIR_HASH_SETS_H

#include <stdint.h>
#include <stdbool.h>

/*
 * - data = array of pairs
 * - size = size of that array (must be a power of 2)
 * - nelems = number of elements in data array
 */
typedef struct intpair_s {
  int32_t left, right;
} int_pair_t;

typedef struct pair_hset_s {
  int_pair_t *data;
  uint32_t size;
  uint32_t nelems;
  uint32_t resize_threshold;
} pair_hset_t;

/*
 * Default initial size
 */
#define PAIR_HSET_DEFAULT_SIZE 64

/*
 * Maximal size
 */
#define PAIR_HSET_MAX_SIZE (UINT32_MAX/sizeof(int_pair_t))

/*
 * Resize ratio: size is doubled when nelems >= size * RESIZE_RATIO
 */
#define PAIR_HSET_RESIZE_RATIO 0.7


/*
 * Initialize
 * - n = initial size. n must be a power of 2
 * - if n is 0, the default size is used
 */
extern void init_pair_hset(pair_hset_t *set, uint32_t n);

/*
 * Delete: free memory
 */
extern void delete_pair_hset(pair_hset_t *set);


/*
 * Check for emptiness
 */
static inline bool pair_hset_is_empty(pair_hset_t *set) {
  return set->nelems == 0;
}

static inline bool pair_hset_is_nonempty(pair_hset_t *set) {
  return ! pair_hset_is_empty(set);
}


/*
 * Check whether the pair <x, y> is in set
 * - both x and y must be non-negative
 */
extern bool pair_hset_member(pair_hset_t *set, int32_t x, int32_t y);


/*
 * Add pair <x, y> to the set
 * - both x and y must be non-negative
 * - return true if the addition was need, i.e., if <x, y> was not present
 * - return false is <x, y> was present (and leave set unchanged)
 */
extern bool pair_hset_add(pair_hset_t *set, int32_t x, int32_t y);


/*
 * Empty the set
 */
extern void pair_hset_reset(pair_hset_t *set);


#endif /* __PAIR_HASH_SETS_H */
