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

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <inttypes.h>
#include <assert.h>

#include "terms/bv_constants.h"
#include "terms/bvarith_buffers.h"
#include "terms/pprod_table.h"
#include "utils/object_stores.h"


/*
 * Display power products
 */
static void print_varexp_array(FILE *f, varexp_t *a, uint32_t n) {
  uint32_t i, d;

  if (n == 0) {
    fprintf(f, "1");
    return;
  }
  d = a[0].exp;
  fprintf(f, "x_%"PRId32, a[0].var);
  if (d != 1) {
    fprintf(f, "^%"PRIu32, d);
  }
  for (i=1; i<n; i++) {
    d = a[i].exp;
    fprintf(f, " x_%"PRId32, a[i].var);
    if (d != 1) {
      fprintf(f, "^%"PRIu32, d);
    }
  }
}

static void print_pprod0(FILE *f, pprod_t *p) {
  if (pp_is_var(p)) {
    fprintf(f, "x_%"PRId32, var_of_pp(p));
  } else if (pp_is_empty(p)) {
    fprintf(f, "1");
  } else {
    print_varexp_array(f, p->prod, p->len);
  }
}




/*
 * Print buffer b
 */
static void print_bv_monomial(FILE *f, uint32_t *coeff, pprod_t *r, uint32_t n, bool first) {
  if (! first) {
    fprintf(f, " + ");
  }
  bvconst_print(f, coeff, n);
  if (! pp_is_empty(r)) {
    fprintf(f, " ");
    print_pprod0(f, r);
  }
}

static void print_bvarith_buffer(FILE *f, bvarith_buffer_t *b) {
  bvmlist_t *p;
  bool first;

  if (bvarith_buffer_is_zero(b)) {
    fprintf(f, "0");
  } else {
    p = b->list;
    first = true;
    while (p->next != NULL) {
      print_bv_monomial(f, p->coeff, p->prod, b->bitsize, first);
      first = false;
      p = p->next;
    }
  }
}



/*
 * Test basic operations: b must be normalized
 */
static void test_buffer_pred(char *s, bvarith_buffer_t *b, bool (*f)(bvarith_buffer_t *)) {
  printf("  test %s: ", s);
  if (f(b)) {
    printf("yes\n");
  } else {
    printf("no\n");
  }
}

static void test_buffer(bvarith_buffer_t *b) {
  bvmlist_t *m;

  printf("Buffer %p: ", b);
  print_bvarith_buffer(stdout, b);
  printf("\n");

  test_buffer_pred("is_zero", b, bvarith_buffer_is_zero);
  test_buffer_pred("is_constant", b, bvarith_buffer_is_constant);
  printf("  size: %"PRIu32"\n", bvarith_buffer_size(b));
  printf("  bitsize: %"PRIu32"\n", bvarith_buffer_bitsize(b));
  printf("  width: %"PRIu32"\n", bvarith_buffer_width(b));
  printf("  degree: %"PRIu32"\n", bvarith_buffer_degree(b));
  if (! bvarith_buffer_is_zero(b)) {
    printf("  main term: ");
    print_pprod0(stdout, bvarith_buffer_main_term(b));
    printf("\n");
    m = bvarith_buffer_main_mono(b);
    printf("  main monomial: ");
    bvconst_print(stdout, m->coeff, b->bitsize);
    printf(" * ");
    print_pprod0(stdout, m->prod);
    printf("\n");
  }

  printf("---\n");
}


/*
 * Global variables:
 * - global prod table and store
 */
static pprod_table_t prod_table;
static object_store_t store;


/*
 * Initialize table and store
 */
static void init_globals(void) {
  init_bvconstants();
  init_bvmlist_store(&store);
  init_pprod_table(&prod_table, 0);
}

/*
 * Delete table and store
 */
static void delete_globals(void) {
  delete_pprod_table(&prod_table);
  delete_bvmlist_store(&store);
  cleanup_bvconstants();
}


/*
 * Tests: one buffer
 * - n = bitsize
 */
static void test1(uint32_t n) {
  bvarith_buffer_t buffer;
  uint32_t q0[4];

  assert(0 < n && n <= 128);

  init_bvarith_buffer(&buffer, &prod_table, &store);
  bvarith_buffer_prepare(&buffer, n);
  printf("Empty buffer\n");
  test_buffer(&buffer);

  printf("x_0 + x_1\n");
  bvarith_buffer_add_var(&buffer, 0);
  bvarith_buffer_add_var(&buffer, 1);
  bvarith_buffer_normalize(&buffer);
  test_buffer(&buffer);

  printf("After reset\n");
  bvarith_buffer_prepare(&buffer, n);
  test_buffer(&buffer);

  printf("x_2 - x_0\n");
  bvarith_buffer_add_var(&buffer, 2);
  bvarith_buffer_sub_var(&buffer, 0);
  bvarith_buffer_normalize(&buffer);
  test_buffer(&buffer);

  printf("x_2 - x_0 + x_1 + x_0\n");
  bvarith_buffer_prepare(&buffer, n);
  bvarith_buffer_add_var(&buffer, 2);
  bvarith_buffer_sub_var(&buffer, 0);
  bvarith_buffer_add_var(&buffer, 1);
  bvarith_buffer_add_var(&buffer, 0);
  bvarith_buffer_normalize(&buffer);
  test_buffer(&buffer);

  printf("Adding 3\n");
  bvconst_set32(q0, 4, 3);
  bvarith_buffer_add_const(&buffer, q0);
  bvarith_buffer_normalize(&buffer);
  test_buffer(&buffer);

  printf("Negating\n");
  bvarith_buffer_negate(&buffer);
  bvarith_buffer_normalize(&buffer);
  test_buffer(&buffer);

  printf("Negating again\n");
  bvarith_buffer_negate(&buffer);
  bvarith_buffer_normalize(&buffer);
  test_buffer(&buffer);

  printf("Multiplying by 2 x_4\n");
  bvconst_set32(q0, 4, 2);
  bvarith_buffer_mul_varmono(&buffer, q0, 4);
  bvarith_buffer_normalize(&buffer);
  test_buffer(&buffer);

  printf("Multiplying by x_1^2\n");
  bvarith_buffer_mul_var(&buffer, 1);
  bvarith_buffer_mul_var(&buffer, 1);
  bvarith_buffer_normalize(&buffer);
  test_buffer(&buffer);

  printf("Multiplying by 0\n");
  bvconst_clear(q0, 4);
  bvarith_buffer_mul_const(&buffer, q0);
  bvarith_buffer_normalize(&buffer);
  test_buffer(&buffer);

  printf("x_1 + 1 - x_2\n");
  bvarith_buffer_prepare(&buffer, n);
  bvarith_buffer_add_var(&buffer, 1);
  bvconst_set32(q0, 4, 1);
  bvarith_buffer_add_const(&buffer, q0);
  bvarith_buffer_sub_var(&buffer, 2);
  bvarith_buffer_normalize(&buffer);
  test_buffer(&buffer);

  printf("Squaring\n");
  bvarith_buffer_square(&buffer);
  bvarith_buffer_normalize(&buffer);
  test_buffer(&buffer);

  printf("Squaring\n");
  bvarith_buffer_square(&buffer);
  bvarith_buffer_normalize(&buffer);
  test_buffer(&buffer);

  printf("Squaring\n");
  bvarith_buffer_square(&buffer);
  bvarith_buffer_normalize(&buffer);
  test_buffer(&buffer);

  delete_bvarith_buffer(&buffer);
}


/*
 * Test2: binary operations
 */

/*
 * Array of buffers for test2
 */
#define NUM_BUFFERS 8
static bvarith_buffer_t aux[NUM_BUFFERS];

/*
 * Initialize the buffers:
 * - n = bitsize
 */
static void init_test2(uint32_t n) {
  uint32_t q0[4];
  uint32_t i;

  assert(0 < n && n <= 128);

  for (i=0; i<8; i++) {
    init_bvarith_buffer(aux + i, &prod_table, &store);
    bvarith_buffer_prepare(aux + i, n);
  }

  bvarith_buffer_add_var(&aux[0], 3); // x_3

  bvconst_set32(q0, 4, 2);
  bvarith_buffer_add_const(&aux[1], q0); // 2

  bvarith_buffer_add_var(&aux[2], 1);
  bvarith_buffer_sub_var(&aux[2], 2); // x_1 - x_2

  bvarith_buffer_add_var(&aux[3], 0);
  bvarith_buffer_sub_const(&aux[3], q0); // x_0 - 2

  bvarith_buffer_add_pp(&aux[4], pprod_mul(&prod_table, var_pp(1), var_pp(1))); // x_1^2

  bvarith_buffer_add_var(&aux[5], 0);
  bvarith_buffer_mul_const(&aux[5], q0); // 2 * x_0

  bvarith_buffer_add_varmono(&aux[6], q0, 1); // 2 * x_1

  bvarith_buffer_sub_var(&aux[7], 3);
  bvarith_buffer_sub_var(&aux[7], 3);
  bvarith_buffer_add_var(&aux[7], 4);

  for (i=0; i<8; i++) {
    bvarith_buffer_normalize(aux + i);
  }
}


/*
 * Delete the buffers
 */
static void delete_test2(void) {
  uint32_t i;

  for (i=0; i<8; i++) {
    delete_bvarith_buffer(aux + i);
  }
}


/*
 * Test binary operations with b1 and b2
 */
static void test_ops(bvarith_buffer_t *b1, bvarith_buffer_t *b2) {
  bvarith_buffer_t b;
  uint32_t n;

  assert(b1->bitsize == b2->bitsize);

  printf("b1: ");
  print_bvarith_buffer(stdout, b1);
  printf("\nb2: ");
  print_bvarith_buffer(stdout, b2);
  printf("\n");

  printf("Equality test: ");
  if (bvarith_buffer_equal(b1, b2)) {
    printf("yes\n");
  } else {
    printf("no\n");
  }

  n = b1->bitsize;
  init_bvarith_buffer(&b, &prod_table, &store);

  bvarith_buffer_prepare(&b, n);
  bvarith_buffer_add_buffer(&b, b1);
  bvarith_buffer_add_buffer(&b, b2);
  bvarith_buffer_normalize(&b);
  printf("  b1 + b2: ");
  print_bvarith_buffer(stdout, &b);
  printf("\n");

  bvarith_buffer_prepare(&b, n);
  bvarith_buffer_add_buffer(&b, b1);
  bvarith_buffer_sub_buffer(&b, b2);
  bvarith_buffer_normalize(&b);
  printf("  b1 - b2: ");
  print_bvarith_buffer(stdout, &b);
  printf("\n");

  bvarith_buffer_prepare(&b, n);
  bvarith_buffer_add_buffer(&b, b2);
  bvarith_buffer_sub_buffer(&b, b1);
  bvarith_buffer_normalize(&b);
  printf("  b2 - b1: ");
  print_bvarith_buffer(stdout, &b);
  printf("\n");

  bvarith_buffer_prepare(&b, n);
  bvarith_buffer_add_buffer(&b, b1);
  bvarith_buffer_mul_buffer(&b, b2);
  bvarith_buffer_normalize(&b);
  printf("  b1 * b2: ");
  print_bvarith_buffer(stdout, &b);
  printf("\n");

  bvarith_buffer_prepare(&b, n);
  bvarith_buffer_add_buffer(&b, b2);
  bvarith_buffer_mul_buffer(&b, b1);
  bvarith_buffer_normalize(&b);
  printf("  b2 * b1: ");
  print_bvarith_buffer(stdout, &b);
  printf("\n");

  bvarith_buffer_prepare(&b, n);
  bvarith_buffer_add_buffer_times_buffer(&b, b1, b2);
  bvarith_buffer_normalize(&b);
  printf("  b1 * b2: ");
  print_bvarith_buffer(stdout, &b);
  printf("\n");

  bvarith_buffer_prepare(&b, n);
  bvarith_buffer_sub_buffer_times_buffer(&b, b1, b2);
  bvarith_buffer_normalize(&b);
  printf("- b1 * b2: ");
  print_bvarith_buffer(stdout, &b);
  printf("\n");

  delete_bvarith_buffer(&b);

  printf("----\n");
}


/*
 * Test 2: n = bitsize
 */
static void test2(uint32_t n) {
  uint32_t i, j;

  init_test2(n);
  for (i=0; i<8; i++) {
    for (j=0; j<8; j++) {
      test_ops(aux + i, aux + j);
    }
  }
  delete_test2();
}


int main(void) {
  init_globals();

  test1(5);
  printf("\n\n");
  test1(32);
  printf("\n\n");
  test1(35);
  printf("\n\n");

  test2(5);
  printf("\n\n");
  test2(32);
  printf("\n\n");
  test2(35);
  printf("\n\n");

  delete_globals();

  return 0;
}
