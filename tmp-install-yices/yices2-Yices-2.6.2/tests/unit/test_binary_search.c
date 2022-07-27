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
 * Binary search in an array of sorted integers
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

#include "utils/cputime.h"
#include "utils/memalloc.h"

#ifdef MINGW

static inline long int random(void) {
  return rand();
}

#endif


/*
 * Search for index of x in array a[0 ... n-1]
 * For sequential search, a[n] must contain INT32_MAX as end marker
 * - return -1 if x is not present
 */
static int32_t binary_search(int32_t *a, uint32_t n, int32_t x) {
  uint32_t l, h, k;

  l = 0;
  h = n;
  if (h == 0) return -1;

  for (;;) {
    k = (l + h)/2;
    assert(l <= k && k < h && h <= n);
    if (k == l) break;
    if (a[k] > x) {
      h = k;
    } else {
      l = k;
    }
  }

  if (a[k] == x) {
    return k;
  } else {
    return -1;
  }
}

static int32_t sequential_search(int32_t *a, uint32_t n, int32_t x) {
  uint32_t k;

  k = 0;
  while (a[k] < x) {
    k ++;
  }

  if (a[k] == x) {
    return k;
  } else {
    return -1;
  }
}



/*
 * Input: must consist of n distinct sorted integers, not equal to INT32_MAX
 */
static void init_array(int32_t *a, uint32_t n) {
  uint32_t i;
  int32_t x;

  x = 5;
  for (i=0; i<n; i++) {
    a[i] = x;
    x += 5;
  }
  a[i] = INT32_MAX;
}



/*
 * Print a test array
 */
static void print_array(int32_t *a, uint32_t n) {
  uint32_t i, l;

  l = 0;
  for (i=0; i<n; i++) {
    printf(" %4"PRId32, a[i]);
    l ++;
    if (l >= 20) {
      printf("\n");
      l = 0;
    }
  }

  if (l > 0) {
    printf("\n");
  }
}


/*
 * Test binary search on array a of size n
 */
static void test_binary_search(int32_t *a, uint32_t n) {
  int32_t x, k, j, top;

  print_array(a, n);
  top = 5 * n + 12;

  for (x=-12; x <top; x ++) {
    k = binary_search(a, n, x);
    j = sequential_search(a, n, x);
    printf("index of %4"PRId32" = %"PRId32"\n", x, k);
    if (j != k || (k>=0 && a[k] != x)) {
      printf("*** BUG ***\n");
    }
  }
}


/*
 * Speed test: binary search
 */
static void speed_test_binary_search(int32_t *a, uint32_t n) {
  double start, done;
  uint32_t i;
  int32_t x, top;

  top = 5 * n + 12;
  printf("Binary search: size = %"PRIu32"   (10000*%"PRId32" searches)\n", n, top+12);
  start = get_cpu_time();
  for (i=0; i<10000; i++) {
    for (x = -12; x<top; x++) {
      (void) binary_search(a, n, x);
    }
  }
  done = get_cpu_time();
  printf("Total time: %.4f\n", done - start);
}


/*
 * Speed test: sequential search
 */
static void speed_test_sequential_search(int32_t *a, uint32_t n) {
  double start, done;
  uint32_t i;
  int32_t x, top;

  top = 5 * n + 12;
  printf("Sequential search: size = %"PRIu32"   (10000*%"PRId32" searches)\n", n, top+12);
  start = get_cpu_time();
  for (i=0; i<10000; i++) {
    for (x = -12; x<top; x++) {
      (void) sequential_search(a, n, x);
    }
  }
  done = get_cpu_time();
  printf("Total time: %.4f\n", done - start);
}





int main(void) {
  int32_t *a;
  uint32_t n;

  for (n=0; n<30; n++) {
    a = (int32_t *) safe_malloc((n+1) * sizeof(int32_t));
    printf("\n=== Test %"PRId32" ===\n", n);
    init_array(a, n);
    test_binary_search(a, n);
    safe_free(a);
  }
  printf("\n");

  // speed test small arrays
  for (n=0; n<100; n++) {
    a = (int32_t *) safe_malloc((n+1) * sizeof(int32_t));
    init_array(a, n);
    speed_test_binary_search(a, n);
    speed_test_sequential_search(a, n);
    printf("\n");
    safe_free(a);
  }

  // larger arrays
  for (n=20; n<2000; n += 200) {
    a = (int32_t *) safe_malloc((n+1) * sizeof(int32_t));
    init_array(a, n);
    speed_test_binary_search(a, n);
    speed_test_sequential_search(a, n);
    printf("\n");
    safe_free(a);
  }

  return 0;
}
