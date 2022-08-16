/*-------------------------------------------------------------------------*/
/* Copyright 2010-2018 Armin Biere Johannes Kepler University Linz Austria */
/*-------------------------------------------------------------------------*/

#include "lglib.h"

#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <signal.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

static LGL * lgl4sigh;
static int catchedsig, verbose, force;

static int * targets, sztargets, ntargets;

static void (*sig_int_handler)(int);
static void (*sig_segv_handler)(int);
static void (*sig_abrt_handler)(int);
static void (*sig_term_handler)(int);
static void (*sig_bus_handler)(int);
static void (*sig_alrm_handler)(int);

static void resetsighandlers (void) {
  (void) signal (SIGINT, sig_int_handler);
  (void) signal (SIGSEGV, sig_segv_handler);
  (void) signal (SIGABRT, sig_abrt_handler);
  (void) signal (SIGTERM, sig_term_handler);
  (void) signal (SIGBUS, sig_bus_handler);
}

static void caughtsigmsg (int sig) {
  if (verbose < 0) return;
  printf ("c\nc CAUGHT SIGNAL %d", sig);
  switch (sig) {
    case SIGINT: printf (" SIGINT"); break;
    case SIGSEGV: printf (" SIGSEGV"); break;
    case SIGABRT: printf (" SIGABRT"); break;
    case SIGTERM: printf (" SIGTERM"); break;
    case SIGBUS: printf (" SIGBUS"); break;
    case SIGALRM: printf (" SIGALRM"); break;
    default: break;
  }
  printf ("\nc\n");
  fflush (stdout);
}

static void catchsig (int sig) {
  if (!catchedsig) {
    catchedsig = 1;
    caughtsigmsg (sig);
    fputs ("c s UNKNOWN\n", stdout);
    fflush (stdout);
    if (verbose >= 0) {
      lglflushtimers (lgl4sigh);
      lglstats (lgl4sigh);
      caughtsigmsg (sig);
    }
  }
  resetsighandlers ();
  if (!getenv ("LGLNABORT")) raise (sig); else exit (1);
}

static void setsighandlers (void) {
  sig_int_handler = signal (SIGINT, catchsig);
  sig_segv_handler = signal (SIGSEGV, catchsig);
  sig_abrt_handler = signal (SIGABRT, catchsig);
  sig_term_handler = signal (SIGTERM, catchsig);
  sig_bus_handler = signal (SIGBUS, catchsig);
}

static int timelimit = -1, caughtalarm = 0;

static void catchalrm (int sig) {
  assert (sig == SIGALRM);
  if (!caughtalarm) {
    caughtalarm = 1;
    caughtsigmsg (sig);
    if (timelimit >= 0) {
      printf ("c time limit of %d reached after %.1f seconds\nc\n",
              timelimit, lglsec (lgl4sigh));
      fflush (stdout);
    }
  }
}

static int checkalarm (void * ptr) {
  assert (ptr == (void*) &caughtalarm);
  return caughtalarm;
}

typedef struct OBuf { char line[81]; int pos; } OBuf;

static void flushobuf (OBuf * obuf, int simponly, FILE * file) {
  assert (0 < obuf->pos);
  assert (obuf->pos < 81);
  obuf->line[obuf->pos++] = '\n';
  assert (obuf->pos < 81);
  obuf->line[obuf->pos++] = 0;
  if (simponly) fputs ("c ", file);
  fputc ('v', file);
  fputs (obuf->line, file);
  obuf->pos = 0;
}

static void print2obuf (OBuf * obuf, int i, int simponly, FILE * file) {
  char str[20];
  int len;
  sprintf (str, " %d", i);
  len = strlen (str);
  assert (len > 1);
  if (obuf->pos + len > 79) flushobuf (obuf, simponly, file);
  strcpy (obuf->line + obuf->pos, str);
  obuf->pos += len;
  assert (obuf->pos <= 79);
}

static FILE * writefile (const char * name, int * clptr) {
  int len = strlen (name);
  char * tmp;
  FILE * res;
  if (len >= 3 && !strcmp (name + len - 3, ".gz")) {
    tmp = malloc (len + 20);
    unlink (name);
    sprintf (tmp, "gzip -c > %s", name);
    res = popen (tmp, "w");
    if (res) *clptr = 2;
    free (tmp);
  } else {
    res = fopen (name, "w");
    if (res) *clptr = 1;
  }
  if (!res) fprintf (stderr, "*** lingeling error: can not write %s\n", name);
  return res;
}

static void closefile (FILE * file, int type) {
  if (type == 1) fclose (file);
  if (type == 2) pclose (file);
}

static void lgltravcounter (void * voidptr, int lit) {
  int * cntptr = voidptr;
  if (!lit) *cntptr += 1;
}

static void lglpushtarget (int target) {
  if (ntargets == sztargets)
    targets = realloc (targets, sizeof *targets *
               (sztargets = sztargets ? 2*sztargets : 1));
  targets[ntargets++] = target;
}

static int primes[] = {
  200000033, 200000039, 200000051, 200000069, 200000081,
};

static int nprimes = sizeof primes / sizeof *primes;

int main (int argc, char ** argv) {
  int res, i, j, clout, val, len, lineno, simponly, count, target;
  const char * iname, * oname, * pname;
  const char * match, * p, * err, * thanks;
  FILE * out, * pfile;
  int maxvar, lit, nopts, simplevel;
  char * tmp;
#ifndef NLGLDRUPLIG
  const char * tname = 0;
  FILE * tfile = 0;
#endif
  LGL * lgl;
  OBuf obuf;
  lineno = 1;
  out = 0;
  res = clout = simponly = simplevel = 0;
  iname = oname = pname = thanks = 0;
  lgl4sigh = lgl = lglinit ();
  setsighandlers ();
  for (i = 1; i < argc; i++) {
    if (!strcmp (argv[i], "-h") || !strcmp (argv[i], "--help")) {
      printf ("usage: lingeling [<option> ...][<file>[.<suffix>]]\n");
      printf ("\n");
      printf ("where <option> is one of the following:\n");
      printf ("\n");
      printf ("-q               be quiet (same as '--verbose=-1')\n");
      printf ("-s               only simplify and print to output file\n");
      printf ("-O<L>            set simplification level to <L>\n");
      printf ("-o <output>      set output file (default 'stdout')\n");
#ifndef NLGLDRUPLIG
      printf ("-t <trace>       set proof trace output file (enable tracing)\n");
#endif
      printf ("-p <options>     read options from file\n");
      printf ("\n");
      printf ("-T <seconds>     set time limit\n");
      printf ("\n");
      printf ("-a <assumption>  use multiple assumptions\n");
      printf ("\n");
      printf ("-h|--help        print command line option summary\n");
      printf ("-f|--force       force reading even without header\n");
      printf ("-r|--ranges      print value ranges of options\n");
      printf ("-d|--defaults    print default values of options\n");
      printf ("-P|--pcs         print (full) PCS file\n");
      printf ("--pcs-mixed      print mixed PCS file\n");
      printf ("--pcs-reduced    print reduced PCS file\n");
      printf ("-e|--embedded    ditto but in an embedded format print\n");
      printf ("-n|--no-witness   do not print solution (see '--witness')\n");
      printf ("\n");
      printf ("-c               increase checking level\n");
      printf ("-l               increase logging level\n");
      printf ("-v               increase verbose level\n");
      printf ("\n");
#ifdef NLGLDRUPLIG
      printf ("--verify         online forward check\n");
      printf ("--proof          generate proof file\n");
#endif
      printf ("\n");
      printf ("--thanks=<whom>  alternative way of specifying the seed\n");
      printf ("                 (inspired by Vampire)\n");
      printf ("\n");
      printf (
"The following options can also be used in the form '--<name>=<int>',\n"
"just '--<name>' for increment and '--no-<name>' for zero.  They\n"
"can be embedded into the CNF file, set through the API or capitalized\n"
"with prefix 'LGL' instead of '--' through environment variables.\n"
"Their default values are displayed in square brackets.\n");
      printf ("\n");
      printf (
"The input <file> can be compressed.  This is detected by matching\n"
"the <suffix> of the filename against 'gz', 'bz2, 'xz', 'zip', '7z'.\n"
"However uncompressing a file is implemented by starting an external\n"
"process running corresponding helper programs, e.g., 'gzip', 'bzip2'.\n"
"Thus those have to be installed and in the current path if needed.\n");
      printf ("\n");
      lglusage (lgl);
      goto DONE;
    } else if (!strcmp (argv[i], "--version")) {
      printf ("%s\n", lglversion ());
      fflush (stdout);
      goto DONE;
    } else if (!strcmp (argv[i], "-s")) simponly = 1;
    else if (argv[i][0] == '-' && argv[i][1] == 'O') {
      if (simplevel > 0) {
	fprintf (stderr, "*** lingeling error: multiple '-O..' options\n");
	res = 1;
	goto DONE;
      }
      if ((simplevel = atoi (argv[i] + 2)) <= 0) {
	fprintf (stderr,
	   "*** lingeling error: invalid '%s' option\n", argv[i]);
	res = 1;
	goto DONE;
      }
    } else if (!strcmp (argv[i], "-q")) lglsetopt (lgl, "verbose", -1);
    else if (!strcmp (argv[i], "-o")) {
      if (++i == argc) {
	fprintf (stderr, "*** lingeling error: argument to '-o' missing\n");
	res = 1;
	goto DONE;
      } 
      if (oname) {
	fprintf (stderr, 
	         "*** lingeling error: "
		 "multiple output files '%s' and '%s'\n",
		 oname, argv[i]);
	res = 1;
	goto DONE;
      }
      oname = argv[i];
    } else if (!strcmp (argv[i], "-p")) {
      if (++i == argc) {
	fprintf (stderr, "*** lingeling error: argument to '-p' missing\n");
	res = 1;
	goto DONE;
      } 
      if (pname) {
	fprintf (stderr, 
	         "*** lingeling error: "
		 "multiple option files '%s' and '%s'\n",
		 pname, argv[i]);
	res = 1;
	goto DONE;
      }
      pname = argv[i];
#ifndef NLGLDRUPLIG
    } else if (!strcmp (argv[i], "-t")) {
      if (++i == argc) {
	fprintf (stderr, "*** lingeling error: argument to '-t' missing\n");
	res = 1;
	goto DONE;
      } 
      if (tname) {
	fprintf (stderr, 
	         "*** lingeling error: "
		 "multiple output files '%s' and '%s'\n",
		 tname, argv[i]);
	res = 1;
	goto DONE;
      }
      tname = argv[i];
#endif
    } else if (!strcmp (argv[i], "-T")) {
      if (++i == argc) {
	fprintf (stderr, "*** lingeling error: argument to '-T' missing\n");
	res = 1;
	goto DONE;
      }
      if (timelimit >= 0) {
	fprintf (stderr, "*** lingeling error: timit limit set twice\n");
	res = 1;
	goto DONE;
      }
      for (p = argv[i]; *p && isdigit ((int)*p); p++) 
	;
      if (p == argv[i] || *p || (timelimit = atoi (argv[i])) < 0) {
	fprintf (stderr, 
	  "*** lingeling error: invalid time limit '-T %s'\n", argv[i]);
	res = 1;
	goto DONE;
      }
    } else if (!strcmp (argv[i], "-a")) {
      if (++i == argc) {
	fprintf (stderr, "*** lingeling error: argument to '-a' missing\n");
	res = 1;
	goto DONE;
      }
      target = atoi (argv[i]);
      if (!target) {
	fprintf (stderr,
	  "*** lingeling error: invalid literal in '-a %d'\n", target);
	res = 1;
	goto DONE;
      }
      lglpushtarget (target);
    } else if (!strcmp (argv[i], "-d") || !strcmp (argv[i], "--defaults")) {
      lglopts (lgl, "", 0);
      goto DONE;
    } else if (!strcmp (argv[i], "-e") || !strcmp (argv[i], "--embedded")) {
      lglopts (lgl, "c ", 1);
      goto DONE;
    } else if (!strcmp (argv[i], "-r") || !strcmp (argv[i], "--ranges")) {
      lglrgopts (lgl);
      goto DONE;
    } else if (!strcmp (argv[i], "-P") || !strcmp (argv[i], "--pcs")) {
      printf ("# generated by 'lingeling --pcs'\n");
      printf ("# version %s\n", lglversion ());
      lglpcs (lgl, 0);
      goto DONE;
    } else if (!strcmp (argv[i], "--pcs-mixed")) {
      printf ("# generated by 'lingeling --pcs-mixed'\n");
      printf ("# version %s\n", lglversion ());
      lglpcs (lgl, 1);
      goto DONE;
    } else if (!strcmp (argv[i], "--pcs-reduced")) {
      printf ("# generated by 'lingeling --pcs-reduced'\n");
      printf ("# version %s\n", lglversion ());
      lglpcs (lgl, -1);
      goto DONE;
    } else if (!strcmp (argv[i], "-f") || !strcmp (argv[i], "--force")) {
      force = 1;
    } else if (!strcmp (argv[i], "-n") || !strcmp (argv[i], "no-witness")) {
      lglsetopt (lgl, "witness", 0);
    } else if (!strcmp (argv[i], "-c")) {
      lglsetopt (lgl, "check", lglgetopt (lgl, "check") + 1);
    } else if (!strcmp (argv[i], "-l")) {
      lglsetopt (lgl, "log", lglgetopt (lgl, "log") + 1);
    } else if (!strcmp (argv[i], "-v")) {
      lglsetopt (lgl, "verbose", lglgetopt (lgl, "verbose") + 1);
#ifndef NLGLDRUPLIG
    } else if (!strcmp (argv[i], "--verify")) {
      lglsetopt (lgl, "druplig", 1);
      lglsetopt (lgl, "drupligcheck", 1);
    } else if (!strcmp (argv[i], "--proof")) {
      lglsetopt (lgl, "druplig", 1);
      lglsetopt (lgl, "drupligtrace", 1);
#endif
    } else if (argv[i][0] == '-') {
      if (argv[i][1] == '-') {
	match = strchr (argv[i] + 2, '=');
	if (match) {
	  p = match + 1;
	  if (*p == '-') p++;	// TODO for what is this useful again?
	  len = p - argv[i];
	  if (!strncmp (argv[i], "--write-api-trace=", len)) {
	    // TODO not handled yet ...
	    continue;
	  } else if (!strncmp (argv[i], "--thanks=", len)) {
	    thanks = match + 1;
	    continue;
	  } else if (!isdigit ((int)*p)) {
ERR:
            fprintf (stderr,
	      "*** lingeling error: invalid command line option '%s'\n",
	      argv[i]);
	    res = 1;
	    goto DONE;
	  }
	  while (*++p) if (!isdigit ((int)*p)) goto ERR;
	  len = match - argv[i] - 2;
	  tmp = malloc (len + 1);
	  j = 0;
	  for (p = argv[i] + 2; *p != '='; p++) tmp[j++] = *p;
	  tmp[j] = 0;
	  val = atoi (match + 1);
	} else if (!strncmp (argv[i], "--no-", 5)) {
	  tmp = strdup (argv[i] + 5);
	  val = 0;
	} else {
	  tmp = strdup (argv[i] + 2);
	  val = lglgetopt (lgl, tmp) + 1;
	}
	if (!lglhasopt (lgl, tmp)) { free (tmp); goto ERR; }
	lglsetopt (lgl, tmp, val);
	free (tmp);
      } else {
	if (argv[i][2]) goto ERR;
	if (!lglhasopt (lgl, argv[i] + 1)) goto ERR;
	val = lglgetopt (lgl, argv[i] + 1) + 1;
	lglsetopt (lgl, argv[i] + 1, val);
      }
    } else if (iname) {
      fprintf (stderr, "*** lingeling error: can not read '%s' and '%s'\n",
               iname, argv[i]);
      res = 1;
      goto DONE;
    } else iname = argv[i];
  }
  verbose = lglgetopt (lgl, "verbose");
  if (verbose >= 0) {
    lglbnr ("Lingeling SAT Solver", "c ", stdout);
    if (simponly) printf ("c simplifying only\n");
    if (oname) printf ("c output file %s\n", oname);
    if (simponly || oname) fflush (stdout);
    lglsetopt (lgl, "trep", 1);
  }
#ifndef NLGLDRUPLIG
  if (tname) {
    tfile = fopen (tname, "w");
    if (!tfile) {
      fprintf (stderr,
         "*** lingeling error: can not write proof trace file %s\n", tname);
      res = 1;
      goto DONE;
    }
    if (verbose >= 0)
      printf ("c proof trace file %s\n", tname), fflush (stdout);
    lglsetrace (lgl, tfile);
    lglsetopt (lgl, "druplig", 1);
    lglsetopt (lgl, "drupligtrace", 2);
  }
#endif
  if (thanks) {
    unsigned seed = 0, i = 0, ch;
    int iseed;
    for (p = thanks; (ch = *p); p++) {
      seed += primes[i++] * ch;
      if (i == nprimes) i = 0;
    }
    if (seed >= (unsigned) INT_MAX) seed >>= 1;
    assert (seed <= (unsigned) INT_MAX);
    iseed = (int) seed;
    assert (iseed >= 0);
    if (verbose)
      printf ("c will have to thank %s (--seed=%d)\nc\n",
	thanks, iseed);
    lglsetopt (lgl, "seed", iseed);
  }
  if (verbose >= 2) {
   printf ("c\nc options after command line parsing:\nc\n");
   lglopts (lgl, "c ", 0);
   printf ("c\n");
   lglsizes (lgl);
   printf ("c\n");
  }
  if (pname) {
    pfile = fopen (pname, "r");
    if (!pfile) {
      fprintf (stderr,
        "*** lingeling error: can not read option file %s\n", pname);
      res = 1;
      goto DONE;
    }
    if (verbose >= 0) {
      printf ("c reading options file %s\n", pname);
      fflush (stdout);
    }
    nopts = lglreadopts (lgl, pfile);
    if (verbose >= 0) 
      printf ("c read and set %d options\nc\n", nopts), fflush (stdout);
    fclose (pfile);
  }
  if (!iname) {
    iname = "<stdin>";
    err = lglparsefile (lgl, stdin, force, &lineno, &maxvar);
  } else err = lglparsepath (lgl, iname, force, &lineno, &maxvar);
  if (err) {
    fprintf (stderr, "%s:%d: %s\n", iname, lineno, err);
    res = 1;
    goto DONE;
  }
  if (verbose >= 1) {
    printf ("c\n");
    if (verbose >= 2) printf ("c final options:\nc\n");
    lglopts (lgl, "c ", 0);
  }
  if (timelimit >= 0) {
    if (verbose >= 0) {
      printf ("c\nc setting time limit of %d seconds\n", timelimit);
      fflush (stdout);
    }
    lglseterm (lgl, checkalarm, &caughtalarm);
    sig_alrm_handler = signal (SIGALRM, catchalrm);
    alarm (timelimit);
  }
  for (i = 0; i < ntargets; i++) lglassume (lgl, targets[i]);
  if (simplevel > 0) {
    if (verbose >= 1) {
      printf ("c simplifying with simplification level %d\n", simplevel);
      fflush (stdout);
    }
    res = lglsimp (lgl, simplevel);
    if (verbose >= 1) {
      printf ("c simplifying result %d after %.2f seconds\n",
	res, lglsec (lgl));
      fflush (stdout);
    }
  }
  res = lglsat (lgl);
  if (timelimit >= 0) {
    caughtalarm = 0;
    (void) signal (SIGALRM, sig_alrm_handler);
  }
  if (oname) {
    double start = lglsec (lgl), delta;
    if (!strcmp (oname, "-")) out = stdout, oname = "<stdout>", clout = 0;
    else if (!(out = writefile (oname, &clout))) { res = 1; goto DONE; }
    if (verbose >= 0) {
      count = 0;
      lglctrav (lgl, &count, lgltravcounter);
      printf ("c\nc writing 'p cnf %d %d' to '%s'\n",
	      maxvar, count, oname);
      fflush (stdout);
    }
    lglprint (lgl, out);
    closefile (out, clout);
    if (verbose >= 0) {
      delta = lglsec (lgl) - start; if (delta < 0) delta = 0;
      printf ("c collected garbage and wrote '%s' in %.1f seconds\n", 
              oname, delta);
      printf ("c\n"), fflush (stdout);
    }
  }
  if (!simponly || verbose >= 0) {
    if (simponly) fputs ("c ", stdout);
    if (res == 10) fputs ("s SATISFIABLE\n", stdout);
    else if (res == 20) fputs ("s UNSATISFIABLE\n", stdout);
    else fputs ("c s UNKNOWN\n", stdout);
    if (thanks) printf ("c\nc Thanks to %s!\nc\n", thanks);
    fflush (stdout);
    if (res == 10 && lglgetopt (lgl, "witness")) {
      obuf.pos = 0;
      for (i = 1; i <= maxvar; i++) {
	lit = (lglderef (lgl, i) > 0) ? i : -i;
	print2obuf (&obuf, lit, simponly, stdout);
      }
      print2obuf (&obuf, 0, simponly, stdout);
      if (obuf.pos > 0) flushobuf (&obuf, simponly, stdout);
      fflush (stdout);
    }
  }
  if (verbose >= 0) fputs ("c\n", stdout), lglstats (lgl);
DONE:
#ifndef NLGLDRUPLIG
  if (tfile) fclose (tfile);
#endif
  resetsighandlers ();
  lgl4sigh = 0;
  lglrelease (lgl);
  free (targets);
  if (verbose > 0) printf ("c exit %d\n", res);
  fflush (stdout);
  return res;
}
