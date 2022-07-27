/**
 *  Btor2Tools: A tool package for the BTOR format.
 *
 *  Copyright (C) 2007-2009 Robert Daniel Brummayer.
 *  Copyright (C) 2007-2012 Armin Biere.
 *  Copyright (C) 2012-2015 Mathias Preiner.
 *  Copyright (c) 2018 Aina Niemetz.
 *
 *  All rights reserved.
 *
 *  This file is part of the Btor2Tools package.
 *  See LICENSE.txt for more information on using this software.
 */

#ifndef BTOR2MEM_H_INCLUDED
#define BTOR2MEM_H_INCLUDED

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*------------------------------------------------------------------------*/

#define BTOR2_NEWN(ptr, nelems) \
  ((ptr) = (typeof(ptr)) btorsim_malloc ((nelems) * sizeof *(ptr)))

#define BTOR2_CNEWN(ptr, nelems) \
  ((ptr) = (typeof(ptr)) btorsim_calloc ((nelems), sizeof *(ptr)))

#define BTOR2_CLRN(ptr, nelems) (memset ((ptr), 0, (nelems) * sizeof *(ptr)))

#define BTOR2_REALLOC(p, n) \
  ((p) = (typeof(p)) btorsim_realloc ((p), ((n) * sizeof *(p))))

#define BTOR2_NEW(ptr) BTOR2_NEWN ((ptr), 1)

#define BTOR2_CNEW(ptr) BTOR2_CNEWN ((ptr), 1)

#define BTOR2_CLR(ptr) BTOR2_CLRN ((ptr), 1)

#define BTOR2_DELETE(ptr) (free (ptr))

static inline void *
btorsim_malloc (size_t size)
{
  void *res;
  if (!size) return 0;
  res = malloc (size);
  if (!res)
  {
    fprintf (stderr, "[btorsim] memory allocation failed\n");
    abort ();
  }
  return res;
}

static inline void *
btorsim_calloc (size_t nobj, size_t size)
{
  void *res;
  res = calloc (nobj, size);
  if (!res)
  {
    fprintf (stderr, "[btorsim] memory allocation failed\n");
    abort ();
  }
  return res;
}

static inline void *
btorsim_realloc (void *p, size_t new_size)
{
  void *res;
  res = realloc (p, new_size);
  if (!res)
  {
    fprintf (stderr, "[btorsim] memory allocation failed\n");
    abort ();
  }
  return res;
}

static inline char *
btorsim_strdup (const char *str)
{
  char *res = 0;
  if (str)
  {
    BTOR2_NEWN (res, strlen (str) + 1);
    strcpy (res, str);
  }
  return res;
}

#endif
