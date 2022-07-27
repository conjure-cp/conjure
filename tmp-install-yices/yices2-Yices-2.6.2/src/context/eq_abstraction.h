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
 * EQUALITY ABSTRACT DOMAIN
 */

/*
 * This module provides support for computing the equalities implied by a
 * formula F. This uses abstract interpretation ideas:
 * - the abstraction of a formula F is the set of equalities (t_1 == t_2)
 *   implied by F
 * - to compute abs(F) we represent the set of equalities as a term partition
 * - this module implements the computation of 'meets' and 'joins' in the
 *   abstract domain.
 *
 * TODO? improve the implementation to take advantage of the new term
 * indexing (i.e., terms are now (index + polarity)).
 */

#ifndef __EQ_ABSTRACTION_H
#define __EQ_ABSTRACTION_H

#include <stdint.h>

#include "terms/terms.h"
#include "utils/int_vectors.h"


/*
 * Compact representation of a term partition:
 * - we just store the classes in an array
 * - each class is separated from the next by an end marker (NULL_TERM)
 * - the partition header contains the number of classes
 *   and the array size
 */
typedef struct epartition_s {
  uint32_t nclasses;
  uint32_t size;   // size of the array data = nterms + nclasses
  term_t data[0];
} epartition_t;


/*
 * Maximal size of a partition
 */
#define EPARTITION_MAX_SIZE ((UINT32_MAX-sizeof(epartition_t))/sizeof(term_t))


/*
 * Auxiliary structure for computing meet and join
 * - for a term t, label[t] = an integer index
 * - each class is stored as a circular list of terms
 *   next[t] = successor of t in its class
 * - for each class i, root[i] is the root of the class
 *   some classes may be marked as empty by setting root[i] to NULL_TERM
 * - subclass = array used for join (to split a class c)
 * The label is interpreted in different ways during meet and join
 * - during a join operation, label[t] = the index of the class of t,
 *   if t is not in any class, we set label[t] = -1
 * - during a meet operation, label[t] = index of t in an epartition object p.
 *   = index of the class of p that contains t.
 */
typedef struct epartition_manager_s {
  uint32_t e_size;  // size of arrays label and next
  uint32_t nterms;  // number of terms such that label[t] >= 0
  int32_t *label;   // maps term to a class index
  term_t *next;

  uint32_t c_size;   // size of the class array
  uint32_t nclasses; // number of classes (<= csize)
  uint32_t order;    // number of nonempty classes
  term_t *root;      // root of each class

  uint32_t sc_size;     // size of the subclass array
  int32_t *subclass;    // maps labels to class id

  // internal buffer
  ivector_t buffer;

  // cache for the empty partition
  epartition_t *empty;
} epartition_manager_t;


/*
 * Maximal and default sizes of these arrays
 */
#define EQABS_DEF_ESIZE 5
#define EQABS_MAX_ESIZE (UINT32_MAX/4)
#define EQABS_DEF_CSIZE 2
#define EQABS_MAX_CSIZE (UINT32_MAX/4)
#define EQABS_DEF_SCSIZE EQABS_DEF_CSIZE
#define EQABS_MAX_SCSIZE EQABS_MAX_CSIZE

/*
 * Initial buffer size
 */
#define EQABS_BUFFER_SIZE 1



/*
 * Initialize manager m:
 * - allocate arrays of default sizes and create the empty partition
 */
extern void init_epartition_manager(epartition_manager_t *m);

/*
 * Deletion: free all data
 */
extern void delete_epartition_manager(epartition_manager_t *m);


/*
 * SIMPLE PARTITIONS
 */

/*
 * Get the empty partition (m must be initialized first)
 */
static inline epartition_t *empty_epartition(epartition_manager_t *m) {
  return m->empty;
}

/*
 * Create a basic partition for (x == y)
 * - one class with two elements x and y (x and y must be distinct)
 */
extern epartition_t *basic_epartition(term_t x, term_t y);


/*
 * Delete a partition
 */
static inline void delete_epartition(epartition_manager_t *m, epartition_t *p) {
  if (p != m->empty) {
    safe_free(p);
  }
}


/*
 * MEET COMPUTATION
 */

/*
 * Initialize m for a meet operation, p = first partition
 * - m must be empty
 */
extern void epartition_init_for_meet(epartition_manager_t *m, epartition_t *p);

/*
 * Store in m the meet of m and p:
 * - the result is the smallest partition that satisfies:
 *   (t == u) in m   implies t == u in result
 *   (t == u) in p   implies t == u in result
 */
extern void epartition_meet(epartition_manager_t *m, epartition_t *p);

/*
 * Convert the partition in m into an epartition object
 * - also reset m to the empty partition
 */
extern epartition_t *epartition_get_meet(epartition_manager_t *m);




/*
 * JOIN COMPUTATION
 */

/*
 * Initialize m for a join operation, p = first partition
 * - m must be empty
 */
extern void epartition_init_for_join(epartition_manager_t *m, epartition_t *p);

/*
 * Store in m the join of m and p:
 * - the result is the coarsest partition that satisfies:
 *   (t == u) in m and (t == u) in p implies (t == u) in result
 */
extern void epartition_join(epartition_manager_t *m, epartition_t *p);

/*
 * Convert the partition in m into an epartition object
 * - also reset m to the empty partition
 */
extern epartition_t *epartition_get_join(epartition_manager_t *m);




#endif /* __EQ_ABSTRACTION_H */
