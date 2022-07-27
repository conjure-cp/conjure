/*-------------------------------------------------------------------------*/
/* Copyright 2010-2018 Armin Biere Johannes Kepler University Linz Austria */
/*-------------------------------------------------------------------------*/

#ifdef NDEBUG
#undef NDEBUG
#endif

#include <assert.h>
#include <ctype.h>
#include <fcntl.h>
#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include "lglib.h"

void lglchkclone (LGL *);

static int runs, golden, timelimit;
static int lineno, verbose, checkreturn, ddopts;
static int nmap = -1, szmap, * map, prevents;
static const char * iname, * oname;

typedef enum Type {
  ADD,ASSUME,DEREF,FAILED,FREEZE,INIT,
  MELT,REUSE,OPTION,PHASE,RELEASE,RETURN,SAT,SIMP,REPR,SETIMPORTANT,
  SETPHASE,RESETPHASE,SETPHASES,FLUSH,REDUCE,FROZEN,USABLE,REUSABLE,
  MAXVAR,INCVAR,FIXED,FIXATE,CHKCLONE,CHANGED,INCONSISTENT,LKHD
} Type;

struct Event;

typedef struct Event {
  Type type;
  int removed, arg;
  char * opt;
} Event;

typedef struct Opt {
  char * name;
  int val, min, max;
} Opt;

static Event * events;
static int nevents, szevents;

static Opt * opts;
static int nopts, szopts;
static long long sumoptvals;

typedef struct Range { int from, to, removed; } Range;

static void event (Type type, int arg, const char * opt) {
  Event * e;
  int idx;
  if (nevents == szevents) {
    szevents = szevents ? 2*szevents : 1;
    events = realloc (events, szevents * sizeof *events);
  }
  e = events + nevents++;
  e->type = type;
  e->arg = arg;
  e->opt = opt ? strdup (opt) : 0;
  e->removed = INT_MAX;

  switch (type) {
    case ADD:
    case ASSUME:
    case DEREF:
    case FAILED:
    case FREEZE:
    case SETIMPORTANT:
    case SETPHASE:
    case RESETPHASE:
    case MELT:
    case REUSE:
    case PHASE:
    case FIXED:
    case FROZEN:
    case REUSABLE:
    case USABLE:
    case REPR:
      idx = abs (e->arg);
      if (idx > nmap) {
	if (idx >= szmap) {
	  do { szmap = szmap ? 2*szmap : 2; } while (szmap <= idx);
	  map = realloc (map, szmap * sizeof *map);
	}
	nmap = idx;
      }
      map[idx] = idx;
      break;
    default:
      break;
  }
}

static void die (const char * fmt, ...) {
  va_list ap;
  fputs ("*** lglddtrace: ", stderr);
  va_start (ap, fmt);
  vfprintf (stderr, fmt, ap);
  va_end (ap);
  fputc ('\n', stderr);
  fflush (stderr);
  exit (1);
}

static void perr (const char * fmt, ...) {
  va_list ap;
  fprintf (stderr,
           "*** lglddtrace: parse error in '%s' line %d: ", iname, lineno);
  va_start (ap, fmt);
  vfprintf (stderr, fmt, ap);
  va_end (ap);
  fputc ('\n', stderr);
  fflush (stderr);
  exit (1);
}

static void rep (const char * fmt, ...) {
  va_list ap;
  if (!verbose) return;
  if (!isatty (1)) return;
  fputs ("c [lglddtrace] ", stdout);
  va_start (ap, fmt);
  vprintf (fmt, ap);
  va_end (ap);
  fputs ("       \r", stdout);
  fflush (stdout);
}

static void msg (const char * fmt, ...) {
  va_list ap;
  if (!verbose) return;
  fputs ("c [lglddtrace] ", stdout);
  va_start (ap, fmt);
  vprintf (fmt, ap);
  va_end (ap);
  fputc ('\n', stdout);
  fflush (stdout);
}

static int isnumstr (const char * str) {
  const char * p;
  int ch;
  if (*(p = str) == '-') p++;
  if (!isdigit ((int)*p++)) return 0;
  while (isdigit (ch = *p)) p++;
  return !ch;
}

static int intarg (char * op) {
  const char * tok;
  if (!(tok = strtok (0, " ")) || !isnumstr (tok) || strtok (0, " "))
    perr ("expected integer argument for '%s'", op);
  assert (tok);
  return atoi (tok);
}

static int remr (Range * r) { return r->removed <= runs; }
static int reme (Event * e) { return e->removed <= runs; }

static void onabort (void * d) { (void) d; exit (0); }

static void process (void) {
  int saved1, saved2, null, tmp, res;
  struct rlimit rlim;
  LGL * lgl;
  Event * e;
  Opt * o;
  rlim.rlim_max = (rlim.rlim_cur = timelimit) + 10;
  setrlimit (RLIMIT_CPU, &rlim);
  saved1 = dup (1);
  saved2 = dup (2);
  null = open ("/dev/null", O_WRONLY);
  close (1);
  close (2);
  tmp = dup (null);
  assert (tmp == 1);
  tmp = dup (null);
  assert (tmp == 2);
  lgl = 0;
  res = 0;
  for (e = events; e < events + nevents; e++) {
    if (reme (e)) continue;
    switch (e->type) {
      case ADD: lgladd (lgl, e->arg); break;
      case ASSUME: lglassume (lgl, e->arg); break;
      case CHKCLONE: lglchkclone (lgl); break;
      case DEREF: res = lglderef (lgl, e->arg); break; 
      case FIXED: res = lglfixed (lgl, e->arg); break;
      case FROZEN: res = lglfrozen (lgl, e->arg); break;
      case REUSABLE: res = lglreusable (lgl, e->arg); break;
      case USABLE: res = lglusable (lgl, e->arg); break;
      case REPR: res = lglrepr (lgl, e->arg); break;
      case FAILED: res = lglfailed (lgl, e->arg); break;
      case FIXATE: lglfixate (lgl); break;
      case REDUCE: lglreducecache (lgl); break;
      case FLUSH: lglflushcache (lgl); break;
      case SETIMPORTANT: lglsetimportant (lgl, e->arg); break;
      case SETPHASES: lglsetphases (lgl); break;
      case SETPHASE: lglsetphase (lgl, e->arg); break;
      case RESETPHASE: lglresetphase (lgl, e->arg); break;
      case FREEZE: lglfreeze (lgl, e->arg); break;
      case INCONSISTENT: res = lglinconsistent (lgl); break;
      case LKHD: res = lglookahead (lgl); break;
      case INCVAR: res = lglincvar (lgl); break;
      case MAXVAR: res = lglincvar (lgl); break;
      case CHANGED: res = lglchanged (lgl); break;
      case INIT: 
        lgl = lglinit ();
	lglonabort (lgl, 0, onabort); 
	if (opts) {
	  assert (ddopts);
	  for (o = opts; o < opts + nopts; o++)
	    lglsetopt (lgl, o->name, o->val);
	}
	break;
      case MELT: lglmelt (lgl, e->arg); break;
      case REUSE: lglreuse (lgl, e->arg); break;
      case OPTION: if (!opts) lglsetopt (lgl, e->opt, e->arg); break;
      case SIMP: res = lglsimp (lgl, e->arg); break;
      case RELEASE: lglrelease (lgl); break;
      case RETURN: 
	if (checkreturn) assert (e->arg == res);
        break;
      case SAT: default:
	assert (e->type == SAT);
	res = lglsat (lgl);
	break;
    }
  }
  close (null);
  close (2);// TODO necessary?
  close (1);// TODO necessary?
  tmp = dup (saved1);
  assert (tmp == 1);
  tmp = dup (saved2);
  assert (tmp == 2);
}

static int run (void) {
  int status = 0, id, tmp;
  // TODO cache
  if ((id = fork ())) {
    if (id < 0) die ("can not generate child process");
    tmp = wait (&status);
    if (tmp != id) die ("'wait' did not return child process");
  } else { process (); exit (0); }
  runs++;
  return status;
}

static int lit (int lit) {
  int idx, res;
  if (!lit) return 0;
  idx = abs (lit);
  assert (0 < idx && idx <= nmap);
  res = map [idx];
  if (lit < 0) res = -res;
  return res;
}

static void print (Event * e, FILE * file) {
  Opt * o;
  switch (e->type) {
    case ADD: fprintf (file, "add %d\n", lit (e->arg)); break;
    case ASSUME: fprintf (file, "assume %d\n", lit (e->arg)); break;
    case CHANGED: fprintf (file, "changed\n"); break;
    case CHKCLONE: fprintf (file, "chkclone\n"); break;
    case DEREF: fprintf (file, "deref %d\n", lit (e->arg)); break;
    case FAILED: fprintf (file, "failed %d\n", lit (e->arg)); break;
    case FIXED: fprintf (file, "fixed %d\n", lit (e->arg)); break;
    case FROZEN: fprintf (file, "frozen %d\n", lit (e->arg)); break;
    case REUSABLE: fprintf (file, "reusable %d\n", lit (e->arg)); break;
    case USABLE: fprintf (file, "usable %d\n", lit (e->arg)); break;
    case REPR: fprintf (file, "repr %d\n", lit (e->arg)); break;
    case FIXATE: fprintf (file, "fixate\n"); break;
    case REDUCE: fprintf (file, "reduce\n"); break;
    case FLUSH: fprintf (file, "flush\n"); break;
    case SETIMPORTANT: fprintf (file, "setimportant %d\n", lit (e->arg)); break;
    case SETPHASES: fprintf (file, "setphases\n"); break;
    case SETPHASE: fprintf (file, "setphase %d\n", lit (e->arg)); break;
    case RESETPHASE: fprintf (file, "resetphase %d\n", lit (e->arg)); break;
    case FREEZE: fprintf (file, "freeze %d\n", lit (e->arg)); break;
    case INCONSISTENT: fprintf (file, "inconsistent\n"); break;
    case LKHD: fprintf (file, "lkhd\n"); break;
    case INCVAR: fprintf (file, "incvar\n"); break;
    case INIT: 
      fprintf (file, "init\n");
      if (opts) {
	assert (ddopts);
	for (o = opts; o < opts + nopts; o++)
	  fprintf (file, "option %s %d\n", o->name, o->val);
      }
      break;
    case MAXVAR: fprintf (file, "maxvar\n"); break;
    case MELT: fprintf (file, "melt %d\n", lit (e->arg)); break;
    case REUSE: fprintf (file, "reuse %d\n", lit (e->arg)); break;
    case OPTION: fprintf (file, "option %s %d\n", e->opt, e->arg); break;
    case PHASE: fprintf (file, "phase %d\n", lit (e->arg)); break;
    case RELEASE: fprintf (file, "release\n"); break;
    case RETURN: fprintf (file, "return %d\n", e->arg); break;
    case SIMP: fprintf (file, "simp %d\n", e->arg); break;
    case SAT: default:
      assert (e->type == SAT);
      fprintf (file, "sat\n");
      break;
  }
}

static const char * type2str (Type type) {
  switch (type) {
    case ADD: return "add";
    case ASSUME: return "assume";
    case CHANGED: return "changed";
    case CHKCLONE: return "chkclone";
    case DEREF: return "deref";
    case FAILED: return "failed";
    case FIXED: return "fixed";
    case FROZEN: return "frozen";
    case REUSABLE: return "reusable";
    case USABLE: return "usable";
    case REPR: return "repr";
    case FIXATE: return "fixate";
    case REDUCE: return "reduce";
    case FLUSH: return "flush";
    case FREEZE: return "freeze";
    case SETIMPORTANT: return "setimportant";
    case SETPHASES: return "setphases";
    case SETPHASE: return "setphase";
    case RESETPHASE: return "resetphase";
    case INCVAR: return "incvar";
    case INCONSISTENT: return "inconsistent";
    case LKHD: return "lkhd";
    case INIT: return "init";
    case MAXVAR: return "maxvar";
    case MELT: return "melt";
    case REUSE: return "REUSE";
    case OPTION: return "option";
    case PHASE: return "phase";
    case RELEASE: return "release";
    case RETURN: return "return";
    case SIMP: return "simp";
    case SAT: default:
      assert (type == SAT);
      return "sat";
      break;
  }
}

static void noarg (Type op) {
  if (strtok (0, " ")) 
    perr ("argument after '%s'", type2str (op));
  event (op, 0, 0);
}

static void newline (void) {
  int i;
  if (!verbose) return;
  if (!isatty (1)) return;
  printf ("\r");
  for (i = 0; i < 78; i++) fputc (' ', stdout);
  printf ("\r");
  fflush (stdout);
}

static void prt (int final) {
  int close = 0, i, len;
  FILE * file; 
  char * cmd;
  unlink (oname);
  len = strlen (oname);
  if (len >= 3 && !strcmp (oname + len - 3, ".gz")) {
    cmd = malloc (len + 20);
    sprintf (cmd, "gzip -c > %s", oname);
    file = popen (cmd, "w");
    if (file) close = 2;
    free (cmd);
  } else {
    file = fopen (oname, "w");
    if (file) close = 1;
  }
  if (!file) die ("can not write to '%s'", oname);
  prevents = 0;
  for (i = 0; i < nevents; i++) {
    if (reme (events + i)) continue;
    print (events + i, file);
    prevents++;
  }
  if (close == 2) pclose (file);
  if (close == 1) fclose (file);
  if (verbose && isatty (1)) {
    fputc ('\r', stdout);
    for (i = 0; i < 78; i++) fputc (' ', stdout);
    fputc ('\r', stdout);
  }
  msg ("written %s with %d events", oname, prevents);
}

static void dd (void) {
  int from, to, rgran, nranges, i, width, j, res, found, pos, changed;
  Range * ranges = calloc (nevents, sizeof *ranges), * r;
  int * smap, idx, moved, mapto, nused;
  Type cluster;
  char * used;
  Event * e;
RESTART:
  prt (0);
  changed = 0;
  nused = 0;
  used = malloc (nmap + 1);
  memset (used, 0, nmap + 1);
  for (e = events; e < events + nevents; e++) {
    if (reme (e)) continue;
    idx = 0;
    switch (e->type) {
      case ADD: 
      case ASSUME: 
      case DEREF:
      case FAILED:
      case FIXED:
      case FROZEN:
      case REUSABLE:
      case USABLE:
      case REPR:
      case SETIMPORTANT:
      case SETPHASE:
      case RESETPHASE:
      case FREEZE:
      case MELT:
      case REUSE:
      case PHASE:
	idx = e->arg;
	break;
      case INIT:
      case OPTION:
      case RELEASE:
      case RETURN: 
      case SAT:
      case SIMP:
      case CHANGED:
      case MAXVAR:
      case INCVAR:
      case INCONSISTENT:
      case LKHD:
      case FIXATE:
      case FLUSH:
      case REDUCE:
      case CHKCLONE:
      case SETPHASES:
        break;
    }
    if (!idx) continue;
    idx = abs (idx);
    if (used [idx]) continue;
    used[idx] = 1;
    nused++;
  }
  if (nused < nmap) {
    smap = map;
    moved = 0;
    mapto = 1;
    map = malloc (szmap * sizeof *map);
    for (idx = 1; idx <= nmap; idx++) {
      if (used[idx]) {
	if (smap[idx] != mapto) {
	  assert (mapto < smap[idx]);
	  moved++;
	}
	map[idx] = mapto++;
      } else map[idx] = 0;
    }
    if (!moved) goto DONOTMOVE;
    res = run ();
    if (res == golden) { 
      if (verbose > 1) {
	newline ();
	msg ("moved %d variables", moved);
      }
      free (smap);
      changed = 1;
    } else {
DONOTMOVE:
      free (map);
      map = smap;
    }
  }
  free (used);
  for (rgran = 2; rgran >= 0; rgran--) {
    from = to = 0;
    r = ranges;
    for (;;) {
      while (from < nevents && reme (events + from))
	from++;
      if (from == nevents) break;
      to = from;
      if (rgran == 2) {
	cluster = events[from].type;
	while (to + 1 < nevents) {
	  e = events + to + 1;
	  if (e->type != cluster) {
	    if (cluster == DEREF && e->type == RETURN) ;
	    else if (cluster == SAT && e->type == RETURN) { to++; break; }
	    else break;
	  } else if (cluster == INIT ||
	             cluster == SAT ||
		     cluster == RELEASE) break;
	  to++;
	}
      } else if (rgran == 1) {
	if (to + 1 < nevents && events[to].type == ADD) {
	  while (to + 1 < nevents &&
		 (reme (e = events + to) || (e->type == ADD && e->arg)))
	    to++;
	}
      } else  {
	if (to + 1 < nevents) {
	  while (to + 1 < nevents && reme (events + to))
	    to++;
	}
      }
      assert (r < ranges + nevents);
      r->from = from;
      r->to = to;
      r->removed = INT_MAX;
      if (verbose > 1) msg ("range %d [%d,%d]", r - ranges, from, to);
      if (verbose > 2) {
	for (i = from; i <= to; i++) {
	  e = events + i;
	  if (reme (e) && verbose < 4) continue;
	  if (e->type == OPTION)
	    msg ("range %d [%d] %s %s %d%s", 
	         r - ranges, i, type2str (e->type), e->opt, e->arg,
		 reme (e) ? " (removed)" : "");
	  else
	    msg ("range %d [%d] %s %d%s", 
		 r - ranges, i, type2str (e->type), e->arg,
		 reme (e) ? " (removed)" : "");
	}
      }
      r++;
      from = to + 1;
      if (from == nevents) break;
    }
    nranges = r - ranges;
    if (verbose > 1)
      msg ("found %d ranges of range granularity %d", nranges, rgran);
    width = nranges / 2;
    while (width > 0) {
      pos = 0;
      do {
	rep ("g%d w%d : %6d .. %-6d / %d %lld",
	     rgran, width, pos, 
	     (pos + width <= nranges) ? pos + width - 1 : nranges - 1,
	     nranges,
	     sumoptvals);
	found = 0;
	for (i = pos; i < nranges && i < pos + width; i++) {
	  r = ranges + i;
	  if (remr (r)) continue;
	  r->removed = runs;
	  found = 0;
	  for (j = r->from; j <= r->to; j++) {
	    e = events + j;
	    if (reme (e)) continue;
	    found++;
	    e->removed = runs;
	  }
	  assert (found);
	}
	res = run ();
	if (res == golden) { 
	  if (verbose > 1) {
	    newline ();
	    msg ("removed %d events", found);
	  }
	  changed = 1;
	} else {
	  for (i = pos; i < nranges && i < pos + width; i++) {
	    r = ranges + i;
	    if (r->removed < runs - 1) continue;
	    assert (r->removed == runs - 1);
	    r->removed = INT_MAX;
	    for (j = r->from; j <= r->to; j++) {
	      e = events + j;
	      assert (e->removed < runs);
	      if (e->removed == runs - 1) e->removed = INT_MAX;
	    }
	  }
	}
	pos += width;
      } while (pos < nranges);
      width = (width > 4) ? width/2 : width - 1;
    }
    if (verbose > 1) newline ();
  }
  if (ddopts) {
    int reported = 0;
    Opt * o;
    if (!opts) {
      void * it;
      const char * name;
      Opt opt;
      LGL * lgl = lglinit ();
      it = lglfirstopt (lgl); 
      while ((it = lglnextopt (lgl, it, 
	                       &name, &opt.val, &opt.min, &opt.max))) {
	if (!strcmp (name, "log")) continue;
	if (!strcmp (name, "check")) continue;
	if (!strcmp (name, "verbose")) continue;
	if (!strcmp (name, "witness")) continue;
	if (!strcmp (name, "exitonabort")) continue;
	if (!strcmp (name, "sleeponabort")) continue;
	if (nopts == szopts) {
	  szopts = szopts ? 2*szopts : 1;
	  opts = realloc (opts, szopts * sizeof *opts);
	}
	opt.name = strdup (name);
	opts[nopts++] = opt;
      }
      for (e = events; e < events + nevents; e++) {
	if (e->type != OPTION) continue;
	for (o = opts; o < opts + nopts; o++)
	  if (!strcmp (e->opt, o->name)) {
	    o->val = e->arg;
	    sumoptvals += o->val - (long long) o->min;
	  }
      }
      lglrelease (lgl);
    }
    for (o = opts; o < opts + nopts; o++) {
      long long delta = o->val - (long long) o->min;
      rep ("o %d / %d %lld                            ",
           o - opts, nopts, sumoptvals);
      reported++;
      if (o->val > o->min) {
	int oldval = o->val;
	o->val = o->min;
	res = run ();
	if (res == golden) {
	  if (verbose > 1) {
	    if (reported) newline ();
	    msg ("reduced option %s from %d to %d by one", 
		 o->name, oldval, o->val);
	    reported = 0;
	  }
	  changed = 1;
	  sumoptvals--;
	} else o->val = oldval;
      }
      if (delta < 10) {
	while (o->val > o->min) {
	  int oldval = o->val, newval = oldval - 1;
	  assert (newval >= o->min);
	  o->val = newval;
	  res = run ();
	  if (res != golden) { o->val = oldval; break; }
	  if (verbose > 1) {
	    if (reported) newline ();
	    msg ("reduced option %s from %d to %d by one", 
		 o->name, oldval, newval);
	    reported = 0;
	  }
	  changed = 1;
	  assert (oldval - newval == 1);
	  sumoptvals--;
	  //assert (sumoptvals >= 0);
	}
      } else {
	int upper = o->val;
	int lower = o->min;
	assert (lower <= upper);
	while (upper > lower) {
	  int oldval = o->val;
	  long longnewval = lower + ((long)upper - (long)lower)/2;
	  int newval = (int) longnewval;
	  o->val = newval;
	  res = run ();
	  if (res == golden) {
	    if (verbose > 1) {
	      if (reported) newline ();
	      msg ("reduced %s from %d to %d", o->name, oldval, newval);
	      reported = 0;
	    }
	    changed = 1;
	    assert (newval < upper);
	    upper = newval;
	    sumoptvals -= oldval - newval;
	    // assert (sumoptvals >= 0);
	  } else {
	    o->val = oldval;
	    if (lower == newval) break;
	    assert (lower < newval);
	    lower = newval;
	  }
	}
	assert (lower <= upper);
      }
      if (verbose > 1) {
	if (reported) newline ();
	msg ("final option %s set to %d", o->name, o->val);
	reported = 0;
      }
    }
    if (reported && verbose > 1) newline ();
  }
  if (changed) goto RESTART;
  if (verbose) newline ();
  free (ranges);
}

static double getime (void) {
  double res = 0;
  struct timeval tv;
  if (!gettimeofday (&tv, 0)) res = 1e-6 * tv.tv_usec, res += tv.tv_sec;
  return res;
}

int main (int argc, char ** argv) {
  int i, len, ch, close = 0, count;
  char buffer[80], * tok, * opt;
  double start, delta;
  FILE * file;
  start = getime ();
  char * cmd;
  for (i = 1; i < argc; i++) {
    if (!strcmp (argv[i], "-h")) {
      printf (
        "usage: lglddtrace [-h][-v][-s][-d][-f][-O] <in>[.gz] <out>[.gz]\n");
      exit (0);
    } else if (!strcmp (argv[i], "-v")) verbose++;
    else if (!strcmp (argv[i], "-c")) checkreturn++;
    else if (!strcmp (argv[i], "-O")) ddopts++;
    else if (argv[i][0] == '-')
      die ("invalid command line option '%s' (try '-h')", argv[i]);
    else if (oname) die ("two many options pecified (try '-h')");
    else if (iname) oname = argv[i];
    else iname = argv[i];
  }
  if (!iname) die ("input file missing");
  if (!oname) die ("output file missing");
  len = strlen (iname);
  if (len >= 3 && !strcmp (iname + len - 3, ".gz")) {
    cmd = malloc (len + 20);
    sprintf (cmd, "gunzip -c %s", iname);
    file = popen (cmd, "r");
    free (cmd);
    if (file) close = 2;
  } else {
    file = fopen (iname, "r");
    if (file) close = 1;
  }
  if (!file) die ("can not read '%s'", iname);
  msg ("reading %s", iname);
  buffer[len = 0] = 0;
  lineno = 1;
  count = 0;
NEXT:
  ch = getc (file);
  if (ch == EOF) goto DONE;
  if (ch == '\r') goto NEXT;
  if (ch != '\n') {
    if (len + 1 >= sizeof (buffer)) perr ("line buffer exceeded");
    buffer[len++] = ch;
    buffer[len] = 0;
    goto NEXT;
  }
  if (verbose > 2) msg ("line %d : %s", lineno, buffer);
  if (!(tok = strtok (buffer, " "))) perr ("empty line");
  else if (!strcmp (tok, "add")) event (ADD, intarg ("add"), 0);
  else if (!strcmp (tok, "return")) event (RETURN, intarg ("return"), 0);
  else if (!strcmp (tok, "deref")) event (DEREF, intarg ("deref"), 0);
  else if (!strcmp (tok, "fixed")) event (FIXED, intarg ("fixed"), 0);
  else if (!strcmp (tok, "frozen")) event (FROZEN, intarg ("frozen"), 0);
  else if (!strcmp (tok, "reusable")) event (REUSABLE, intarg ("reusable"), 0);
  else if (!strcmp (tok, "usable")) event (USABLE, intarg ("usable"), 0);
  else if (!strcmp (tok, "repr")) event (REPR, intarg ("repr"), 0);
  else if (!strcmp (tok, "failed")) event (FAILED, intarg ("failed"), 0);
  else if (!strcmp (tok, "assume")) event (ASSUME, intarg ("assume"), 0);
  else if (!strcmp (tok, "phase")) event (PHASE, intarg ("phase"), 0);
  else if (!strcmp (tok, "init")) noarg (INIT);
  else if (!strcmp (tok, "sat")) noarg (SAT);
  else if (!strcmp (tok, "simp")) event (SIMP, intarg ("simp"), 0);
  else if (!strcmp (tok, "setphases")) noarg (SETPHASES);
  else if (!strcmp (tok, "freeze")) event (FREEZE, intarg ("freeze"), 0);
  else if (!strcmp (tok, "setimportant"))
    event (SETIMPORTANT, intarg ("setimportant"), 0);
  else if (!strcmp (tok, "setphase")) event (SETPHASE, intarg ("setphase"), 0);
  else if (!strcmp (tok, "resetphase"))
    event (SETPHASE, intarg ("resetphase"), 0);
  else if (!strcmp (tok, "melt")) event (MELT, intarg ("melt"), 0);
  else if (!strcmp (tok, "reuse")) event (REUSE, intarg ("reuse"), 0);
  else if (!strcmp (tok, "option")) {
    if (!(opt = strtok (0, " "))) perr ("option iname missing");
    event (OPTION, intarg ("option"), opt);
  } else if (!strcmp (tok, "release")) noarg (RELEASE);
  else if (!strcmp (tok, "incvar")) noarg (INCVAR);
  else if (!strcmp (tok, "maxvar")) noarg (MAXVAR);
  else if (!strcmp (tok, "inconsistent")) noarg (INCONSISTENT);
  else if (!strcmp (tok, "lkhd")) noarg (LKHD);
  else if (!strcmp (tok, "fixate")) noarg (FIXATE);
  else if (!strcmp (tok, "reduce")) noarg (REDUCE);
  else if (!strcmp (tok, "flush")) noarg (FLUSH);
  else if (!strcmp (tok, "chkclone")) noarg (CHKCLONE);
  else if (!strcmp (tok, "changed")) noarg (CHANGED);
  else perr ("invalid command '%s'", tok);
  lineno++;
  count++;
  len = 0;
  goto NEXT;
DONE:
  if(close == 1) fclose (file);
  if(close == 2) pclose (file);
  msg ("parsed %d events in %s", count, iname);
  golden = run ();
  timelimit = 
  delta = getime () - start;
  if (delta < 0) delta = 0;
  msg ("golden exit code %d in %.3f seconds", golden, delta);
  timelimit = delta;
  if (timelimit <= 0) timelimit = 1;
  timelimit *= 100;
  msg ("time limit %d seconds", timelimit);
  dd ();
  prt (1);
  for (i = 0; i < nevents; i++) free (events[i].opt);
  free (events);
  for (i = 0; i < nopts; i++) free (opts[i].name);
  free (opts);
  msg ("executed %d runs in %.3f seconds", runs, getime () - start);
  return 0;
}
