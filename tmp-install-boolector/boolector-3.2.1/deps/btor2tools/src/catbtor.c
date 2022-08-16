/**
 *  Btor2Tools: A tool package for the BTOR format.
 *
 *  Copyright (c) 2012-2015 Armin Biere.
 *  Copyright (c) 2017 Mathias Preiner.
 *  Copyright (c) 2017-2018 Aina Niemetz.
 *
 *  All rights reserved.
 *
 *  This file is part of the Btor2Tools package.
 *  See LICENSE.txt for more information on using this software.
 */

#include "btor2parser/btor2parser.h"

#include <assert.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int32_t close_input;
static FILE* input_file;
static const char* input_name;

/* Parse BTOR2 file and print to stdout. */

int32_t
main (int32_t argc, char** argv)
{
  Btor2Parser* reader;
  Btor2LineIterator it;
  Btor2Line* l;
  uint32_t j;
  int32_t i, verbosity = 0;
  const char* err;
  for (i = 1; i < argc; i++)
  {
    if (!strcmp (argv[i], "-h"))
    {
      fprintf (stderr, "usage: catbtor [-h|-v] [ <btorfile> ]\n");
      exit (1);
    }
    else if (!strcmp (argv[i], "-v"))
      verbosity++;
    else if (argv[i][0] == '-')
    {
      fprintf (
          stderr, "*** catbtor: invalid option '%s' (try '-h')\n", argv[i]);
      exit (1);
    }
    else if (input_name)
    {
      fprintf (stderr, "*** catbtor: too many inputs (try '-h')\n");
      exit (1);
    }
    else
      input_name = argv[i];
  }
  if (!input_name)
  {
    input_file = stdin;
    assert (!close_input);
    input_name = "<stdin>";
  }
  else
  {
    input_file = fopen (input_name, "r");
    if (!input_file)
    {
      fprintf (
          stderr, "*** catbtor: can not open '%s' for reading\n", input_name);
      exit (1);
    }
    close_input = 1;
  }
  if (verbosity)
  {
    fprintf (stderr,
             "; [catbor] simple CAT for BTOR files\n"
             "; [catbor] reading '%s'\n",
             input_name);
    fflush (stderr);
  }
  reader = btor2parser_new ();
  if (!btor2parser_read_lines (reader, input_file))
  {
    err = btor2parser_error (reader);
    assert (err);
    fprintf (stderr, "*** catbtor: parse error in '%s' %s\n", input_name, err);
    btor2parser_delete (reader);
    if (close_input) fclose (input_file);
    exit (1);
  }
  if (close_input) fclose (input_file);
  if (verbosity)
  {
    fprintf (stderr, "; [catbor] finished parsing '%s'\n", input_name);
    fflush (stderr);
  }
  if (verbosity)
  {
    fprintf (stderr, "; [catbor] starting to dump BTOR model to '<stdout>'\n");
    fflush (stderr);
  }
  it = btor2parser_iter_init (reader);
  while ((l = btor2parser_iter_next (&it)))
  {
    printf ("%" PRId64 " %s", l->id, l->name);
    if (l->tag == BTOR2_TAG_sort)
    {
      printf (" %s", l->sort.name);
      switch (l->sort.tag)
      {
        case BTOR2_TAG_SORT_bitvec: printf (" %u", l->sort.bitvec.width); break;
        case BTOR2_TAG_SORT_array:
          printf (" %" PRId64 " %" PRId64, l->sort.array.index, l->sort.array.element);
          break;
        default:
          assert (0);
          fprintf (stderr, "*** catbtor: invalid sort encountered\n");
          exit (1);
      }
    }
    else if (l->sort.id)
      printf (" %" PRId64, l->sort.id);
    for (j = 0; j < l->nargs; j++) printf (" %" PRId64, l->args[j]);
    if (l->tag == BTOR2_TAG_slice) printf (" %" PRId64 " %" PRId64, l->args[1], l->args[2]);
    if (l->tag == BTOR2_TAG_sext || l->tag == BTOR2_TAG_uext)
      printf (" %" PRId64, l->args[1]);
    if (l->constant) printf (" %s", l->constant);
    if (l->symbol) printf (" %s", l->symbol);
    fputc ('\n', stdout);
  }
  btor2parser_delete (reader);
  if (verbosity)
  {
    fprintf (stderr, "; [catbor] finished dumping BTOR model to '<stdout>'\n");
    fflush (stderr);
  }
  return 0;
}
