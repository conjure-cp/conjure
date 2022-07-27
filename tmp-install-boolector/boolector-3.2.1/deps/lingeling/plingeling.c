/*-------------------------------------------------------------------------*/
/* Copyright 2010-2018 Armin Biere Johannes Kepler University Linz Austria */
/*-------------------------------------------------------------------------*/

#include "lglib.h"

#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <stdarg.h>
#include <signal.h>
#include <unistd.h>
#include <math.h>

/*------------------------------------------------------------------------*/

#define NWORKERS 8
#define MAXGB 12
#define NUNITS (1<<9)

/*------------------------------------------------------------------------*/
#if 0
static int memdbgcnt;
#define LOGMEM(TYPE,PTR,BYTES) \
do { \
  fprintf (stderr, "%ld %10d " #TYPE " %ld\n", \
    (long) PTR, memdbgcnt++, (long) BYTES); \
  fflush (stderr); \
} while (0)
#else
#define LOGMEM(...) do { } while (0)
#endif
/*------------------------------------------------------------------------*/

#define NEW(PTR,NUM) \
do { \
  size_t BYTES = (NUM) * sizeof *(PTR); \
  if (!((PTR) = malloc (BYTES))) { die ("out of memory"); exit (1); } \
  LOGMEM (a, (PTR), BYTES); \
  memset ((PTR), 0, BYTES); \
  incmem (BYTES); \
} while (0)

#define DEL(PTR,NUM) \
do { \
  size_t BYTES = (NUM) * sizeof *(PTR); \
  LOGMEM (d, (PTR), BYTES); \
  decmem (BYTES); \
  free (PTR); \
} while (0)

/*------------------------------------------------------------------------*/

typedef struct Cls { int wid, count, glue, lits[1]; } Cls;

typedef struct Worker {
  LGL * lgl;
  pthread_t thread;
  int res, fixed;
  int units[NUNITS], nunits;
  int * cls, szcls;
  long clsimported;
  Cls * dead;
  struct {
    struct { int calls, produced, consumed; } units;
    struct { int produced, consumed; } cls, eqs;
    int produced, consumed;
  } stats;
} Worker;

/*------------------------------------------------------------------------*/

static int verbose, plain, nounits, noeqs, nocls, gclim = 1, gclimset;
#ifndef NLGLOG
static int loglevel;
#endif
#ifndef NLGLYALSAT
static int forcelocs;
#endif

/*------------------------------------------------------------------------*/

static int nworkers, nconsumers, leavebehind, locs;
static int64_t memlimit, softmemlimit;
static Worker * workers;
static int nvars, nclauses;
static int * vals, * fixed, * repr;
struct { Cls ** start; long first, num, added, collected, size; } clauses;
static int nfixed, globalres, gcs;
static const char * name;
static int nworkers2;
struct { size_t max, current;} mem;
static int catchedsig;
static double start;
static FILE * file;

struct { int units, cls, eqs; } syncs;
static int done, termchks, units, eqs, flushed;
static pthread_mutex_t donemutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t msgmutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t fixedmutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t reprmutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t memutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t clsmutex = PTHREAD_MUTEX_INITIALIZER;

/*------------------------------------------------------------------------*/

static double currentime (void) {
  double res = 0;
  struct timeval tv;
  if (!gettimeofday (&tv, 0)) res = 1e-6 * tv.tv_usec, res += tv.tv_sec;
  return res;
}

static double getime () { 
  return currentime () - start; 
}

static void msg (int wid, int level, const char * fmt, ...) {
  va_list ap;
  if (verbose < level) return;
  pthread_mutex_lock (&msgmutex);
  fflush (stderr);
  if (wid < 0) printf ("c - "); else printf ("c %d ", wid);
  printf ("W %6.1f ", getime ());
  va_start (ap, fmt);
  vfprintf (stdout, fmt, ap);
  fputc ('\n', stdout);
  fflush (stdout);
  pthread_mutex_unlock (&msgmutex);
}

static void die (const char * fmt, ...) {
  va_list ap;
  fflush (stdout);
  fputs ("c *** plingeling error: ", stderr);
  va_start (ap, fmt);
  vfprintf (stderr, fmt, ap);
  fputc ('\n', stderr);
  fflush (stderr);
  exit (1);
}

static void warn (const char * fmt, ...) {
  va_list ap;
  fflush (stdout);
  fputs ("c *** plingeling warning: ", stderr);
  va_start (ap, fmt);
  vfprintf (stderr, fmt, ap);
  fputc ('\n', stderr);
  fflush (stderr);
}

/*------------------------------------------------------------------------*/

static double percent (double a, double b) { return b ? (100 * a) / b : 0; }

static void stats (void) {
  double real, process, mpps, cps, mb;
  int64_t decs, confs, props;
  int i, unitcalls;
  Worker * w;
  unitcalls = decs = confs = 0;
  props = 0;
  mb = mem.max / (double)(1<<20);
  for (i = 0; i < nworkers; i++) {
    w = workers + i;
    if (!w->lgl) continue;
    decs += lglgetdecs (w->lgl);
    confs += lglgetconfs (w->lgl);
    props += lglgetprops (w->lgl);
    mb += lglmaxmb (w->lgl);
    unitcalls += w->stats.units.calls;
  }
  real = getime ();
  process = lglprocesstime ();
  cps = real > 0 ? confs / real : 0;
  mpps = real > 0 ? (props/1e6) / real : 0;
  printf ("c %d termination checks\n", termchks);
  printf ("c\n");
  printf ("c units: %d found, %d publications, %d syncs, %d flushed\n", 
          units, unitcalls, syncs.units, flushed);
  printf ("c clauses: %ld clauses added, %ld collected %.0f%%, %d gcs\n",
    clauses.added,
    clauses.collected, percent (clauses.collected, clauses.added),
    gcs);
  printf ("c equivalences: %d found, %d syncs\n", eqs, syncs.eqs);
  printf ("c\n");
  printf ("c %lld decisions, %lld conflicts, %.1f conflicts/sec\n", 
          (long long)decs, (long long)confs, cps);
  printf ("c %lld0 propagations, %.1f megaprops/sec\n",
          (long long)props, mpps);
  printf ("c %.1f process time, %.0f%% utilization\n",
          process, real > 0 ? (100.0 * process) / real / nworkers : 0.0);
  printf ("c\n");
  printf ("c %.1f seconds, %.1f MB\n", real, mb);
  fflush (stdout);
}

/*------------------------------------------------------------------------*/

static void incmem (size_t bytes) {
  if (pthread_mutex_lock (&memutex))
    warn ("failed to lock 'mem' mutex in 'incmem'");
  mem.current += bytes;
  if (mem.current > mem.max) mem.max = mem.current;
  if (pthread_mutex_unlock (&memutex))
    warn ("failed to unlock 'mem' mutex in 'incmem'");
}

static void decmem (size_t bytes) {
  if (pthread_mutex_lock (&memutex))
    warn ("failed to lock 'mem' mutex in 'decmem'");
  assert (mem.current >= bytes);
  mem.current -= bytes;
  if (pthread_mutex_unlock (&memutex))
    warn ("failed to unlock 'mem' mutex in 'decmem'");
}

static void * alloc (void * dummy, size_t bytes) {
  char * res;
  NEW (res, bytes);
  return res;
}

static void dealloc (void * dummy, void * void_ptr, size_t bytes) {
  char * char_ptr = void_ptr;
  LOGMEM (d, void_ptr, bytes);
  DEL (char_ptr, bytes);
}

static void * resize (void * dummy, void * ptr, 
                      size_t old_bytes, size_t new_bytes) {
  void * res;
  if (pthread_mutex_lock (&memutex))
    warn ("failed to lock 'mem' mutex in 'resize'");
  assert (mem.current >= old_bytes);
  mem.current -= old_bytes;
  mem.current += new_bytes;
  if (mem.current > mem.max) mem.max = mem.current;
  if (pthread_mutex_unlock (&memutex))
    warn ("failed to unlock 'mem' mutex in 'resize'");
  LOGMEM (d, ptr, old_bytes);
  res = realloc (ptr, new_bytes);
  LOGMEM (a, res, new_bytes);
  return res;
}

/*------------------------------------------------------------------------*/

static void (*sig_int_handler)(int);
static void (*sig_segv_handler)(int);
static void (*sig_abrt_handler)(int);
static void (*sig_term_handler)(int);

static void resetsighandlers (void) {
  (void) signal (SIGINT, sig_int_handler);
  (void) signal (SIGSEGV, sig_segv_handler);
  (void) signal (SIGABRT, sig_abrt_handler);
  (void) signal (SIGTERM, sig_term_handler);
}

static void caughtsigmsg (int sig) {
  if (!verbose) return;
  printf ("c\nc CAUGHT SIGNAL %d\nc\n", sig);
  fflush (stdout);
}

static void catchsig (int sig) {
  if (!catchedsig) {
    fputs ("c s UNKNOWN\n", stdout);
    fflush (stdout);
    catchedsig = 1;
    caughtsigmsg (sig);
    stats ();
    caughtsigmsg (sig);
  }
  resetsighandlers ();
  if (getenv ("LGLSLEEPONABORT")) {
    const int sec = 1000;
    fprintf (stderr,
      "plingeling: plingeling.c:%d: "
      "process %ld sleeping for %d seconds\n",
      __LINE__, (long) getpid (), sec); fflush (stderr); sleep (sec);
  }
  if (!getenv ("LGLNABORT")) raise (sig); else exit (1);
}

static void setsighandlers (void) {
  sig_int_handler = signal (SIGINT, catchsig);
  // sig_segv_handler = signal (SIGSEGV, catchsig);
  sig_abrt_handler = signal (SIGABRT, catchsig);
  sig_term_handler = signal (SIGTERM, catchsig);
}

/*------------------------------------------------------------------------*/

static const char * parse (void) {
  int ch, lit, sign, minlen, maxlen, len, * occs;
HEADER:
  ch = getc (file);
  if (ch == 'c') {
    while ((ch = getc (file)) != '\n')
      if (ch == EOF) return "EOF in comment";
    goto HEADER;
  }
  if (ch != 'p') return "expected header or comment";
  ungetc (ch, file);
  if (fscanf (file, "p cnf %d %d", &nvars, &nclauses) != 2)
    return "can not parse header";
  msg (-1, 1, "p cnf %d %d", nvars, nclauses);
  NEW (fixed, nvars + 1);
  NEW (vals, nvars + 1);
  NEW (occs, 2*nvars + 1);
  occs += nvars;
  if (!noeqs) NEW (repr, nvars + 1);
  minlen = INT_MAX, maxlen = -1, len = 0;
LIT:
  ch = getc (file);
  if (ch == 'c') {
    while ((ch = getc (file)) != '\n')
      if (ch == EOF) return "EOF in comment";
    goto LIT;
  }
  if (ch == ' ' || ch == '\n' || ch == '\r' || ch == '\t') goto LIT;
  if (ch == EOF) {
    double avg, std;
    if (nclauses > 0) return "not enough clauses";
    if (minlen == maxlen) msg (-1, 0, "uniform clause length %d", minlen);
    else msg (-1, 0, "clause length between %d and %d", minlen, maxlen);
    if (nvars > 0) {
      avg = 0;
      for (lit = -nvars; lit <= nvars; lit++)
	if (lit) avg += occs[lit];
      avg /= 2*nvars;
      msg (-1, 0, "average literal occurrence %.2f", avg);
      std = 0;
      for (lit = -nvars; lit <= nvars; lit++) 
        if (lit) {
	  double diff = avg - (double) occs[lit];
	  std += diff*diff;
	}
      std = sqrt (std / (2*nvars));
      msg (-1, 0, "literal occurrence standard deviation %.2f", std);
      if (avg > 0)
	 msg (-1,  0,
	   "relative literal occurrence standard deviation %.2f%%",
	   100.0*std/avg);
    } else avg = std = 0;
    occs -= nvars;
    DEL (occs, 2*nvars + 1);
    if (avg > 5 && std/avg < 0.5) {
      if (minlen == maxlen) {
	locs = 2;
	msg (-1, 0, "looks like a real uniform random instance");
      } else {
	locs = 1;
	msg (-1, 0,
	  "looks like random instance without uniform clause length");
      }
    } else {
      locs = 0;
      msg (-1, 0, "instance does not seem to be uniform random");
    }
    msg (-1, 1, "finished parsing in %.1f seconds", getime ());
    return 0;
  }
  if (ch == '-') {
    ch = getc (file);
    sign = -1;
  } else sign = 1;
  if (!isdigit (ch)) return "expected digit";
  if (!nclauses) return "too many clauses";
  lit = ch - '0';
  while (isdigit (ch = getc (file)))
    lit = 10 * lit + (ch - '0');
  if (lit < 0 || lit > nvars) return "invalid variable index";
  lit *= sign;
  lgladd (workers[0].lgl, lit);
  if (!lit) {
    nclauses--;
    if (len > maxlen) maxlen = len;
    if (len < minlen) minlen = len;
    len = 0;
  } else len++, occs[lit]++;
  goto LIT;
}

static int isposnum (const char * str) {
  int ch;
  if (!(ch = *str++) || !isdigit (ch)) return 0;
  while (isdigit (ch = *str++))
    ;
  return !ch;
}

static int term (void * voidptr) {
  Worker * worker = voidptr;
  int wid = worker - workers, res;
  assert (0 <= wid && wid < nworkers);
  msg (wid, 3, "checking early termination");
  if (pthread_mutex_lock (&donemutex))
    warn ("failed to lock 'done' mutex in termination check");
  res = done;
  termchks++;
  if (pthread_mutex_unlock (&donemutex)) 
    warn ("failed to unlock 'done' mutex in termination check");
  msg (wid, 3, "early termination check %s", res ? "succeeded" : "failed");
  return res;
}

static void flush (Worker * worker, int keep_locked) {
  int wid = worker - workers;
  int lit, idx, val, tmp, i;
  assert (worker->nunits);
  msg (wid, 2, "flushing %d units", worker->nunits);
  if (pthread_mutex_lock (&fixedmutex))
    warn ("failed to lock 'fixed' mutex in flush");
  flushed++;
  for (i = 0; i < worker->nunits; i++) {
    lit = worker->units[i];
    idx = abs (lit);
    assert (1 <= idx && idx <= nvars);
    assert (0 <= wid && wid < nworkers);
    worker->stats.units.calls++;
    val = (lit < 0) ? -1 : 1;
    tmp = vals[idx];
    if (!tmp) {
      assert (nfixed < nvars);
      fixed[nfixed++] = lit;
      vals[idx] = val;
      assert (!fixed[nfixed]);
      worker->stats.units.produced++;
      worker->stats.produced++;
      units++;
    } else if (tmp == -val) {
      if (pthread_mutex_lock (&donemutex))
	warn ("failed to lock 'done' mutex flushing unit");
      if (!globalres) msg (wid, 1, "mismatched unit");
      globalres = 20;
      done = 1;
      if (pthread_mutex_unlock (&donemutex)) 
	warn ("failed to unlock 'done' mutex flushing unit");
      break;
    } else assert (tmp == val);
  }
  worker->nunits = 0;
  if (keep_locked) return;
  if (pthread_mutex_unlock (&fixedmutex)) 
    warn ("failed to unlock 'fixed' mutex in flush");
}

static void produceunit (void * voidptr, int lit) {
  Worker * worker = voidptr;
  int wid = worker - workers;
  assert (worker->nunits < NUNITS);
  worker->units[worker->nunits++] = lit;
  msg (wid, 3, "producing unit %d", lit);
  if (worker->nunits == NUNITS) flush (worker, 0);
}

static void consumeunits (void * voidptr, int ** fromptr, int ** toptr) {
  Worker * worker = voidptr;
  int wid = worker - workers;
  if (worker->nunits) flush (worker, 1);
  else if (pthread_mutex_lock (&fixedmutex))
    warn ("failed to lock 'fixed' mutex in consume");
  msg (wid, 3, "starting unit synchronization");
  syncs.units++;
  *fromptr = fixed + worker->fixed;
  *toptr = fixed + nfixed;
  if (pthread_mutex_unlock (&fixedmutex))
    warn ("failed to unlock 'fixed' in 'consumeunits'");
}

static void consumedunits (void * voidptr, int consumed) {
  Worker * worker = voidptr;
  int wid = worker - workers;
  worker->stats.units.consumed += consumed;
  worker->stats.consumed += consumed;
  msg (wid, 3, "consuming %d units", consumed);
}

static void * resize (void*,void*,size_t,size_t);

static size_t sizecls (int len) { return sizeof (Cls) + len * sizeof (int); }

static int lencls (int * c) { int res = 0; while (*c++) res++; return res; }

static void producecls (void * voidptr, int * c, int glue) {
  Worker * worker = voidptr;
  int wid = worker - workers;
  int len, * q, lit;
  const int * p;
  size_t bytes;
  Cls * cls;
  len = lencls (c);
  bytes = sizecls (len);
  cls = malloc (bytes);
  if (!cls) { die ("out of memory in 'producecls'"); exit (1); }
  LOGMEM (a, cls, bytes);
  incmem (bytes);
  cls->wid = wid;
  cls->glue = glue;
  cls->count = 0;
  p = c, q = cls->lits; 
  while ((lit = *p++)) *q++ = lit;
  *q++ = 0;
  if (pthread_mutex_lock (&clsmutex))
    warn ("failed to lock 'cls' mutex in 'producecls'");
  msg (wid, 3, "producing glue %d length %d clause %ld",
    glue, len, clauses.added);
  if (clauses.num == clauses.size) {
    int newsize = clauses.size ? 2*clauses.size : 1;
    size_t old_bytes = clauses.size * sizeof *clauses.start;
    size_t new_bytes = newsize * sizeof *clauses.start;
    clauses.start = resize (0, clauses.start, old_bytes, new_bytes);
    clauses.size = newsize;
  }
  clauses.start[clauses.num++] = cls;
  clauses.added++;
  if (pthread_mutex_unlock (&clsmutex))
    warn ("failed to unlock 'cls' mutex in 'producecls'");
  worker->stats.cls.produced++;
  worker->stats.produced++;
}

static void deletecls (Cls * cls) {
  int len = lencls (cls->lits);
  size_t bytes = sizecls (len);
  LOGMEM (d, cls, bytes);
  decmem (bytes);
  free (cls);
}

static void deleteallcls () {
  Cls * c;
  int i;
  for (i = clauses.first; i < clauses.num; i++) {
    c = clauses.start[i];
    if (c) deletecls (c);
  }
  DEL (clauses.start, clauses.size);
}

static void gcls (void) {
  long j, k, l;
  Cls * c;
  int i;
  assert (0 <= clauses.first);
  assert (clauses.first <= clauses.num);
  gcs++;
  msg (-1, 1, "garbage collecting clauses: first=%ld, num=%ld, SIZE=%ld",
    clauses.first, clauses.num, clauses.size);
  for (k = 0; k < clauses.first; k++) assert (!clauses.start[k]);
  while (k < clauses.num) {
    c = clauses.start[k];
    if (c->count > gclim) break;
    clauses.start[k++] = 0;
    deletecls (c);
  }
  assert (clauses.first <= k);
  for (i = 0; i < nworkers; i++) {
    Worker * worker = workers + i;
    if (k < worker->clsimported) worker->clsimported -= k;
    else worker->clsimported = 0;
  }
  j = 0;
  for (l = k; l < clauses.num; l++)
    clauses.start[j++] = clauses.start[l];
  clauses.collected += k;
  clauses.first = 0;
  clauses.num -= k;
  assert (clauses.num >= 0);
}

static void consumecls (void * voidptr, int ** cptr, int * glueptr) {
  Worker * worker = voidptr;
  int wid = worker - workers;
  int res, len, newsize;
  Cls * cls;
  if (pthread_mutex_lock (&clsmutex))
    warn ("failed to lock 'cls' mutex in 'consumecls'");
  assert (worker->clsimported <= clauses.num);
RESTART:
  if (worker->dead) deletecls (worker->dead), worker->dead = 0;
  if (worker->clsimported == clauses.num) {
    msg (wid, 3, "all %d clauses already consumed", clauses.num);
    *cptr = 0;
  } else {
    res = worker->clsimported++;
    cls = clauses.start[res];
    if (!cls) goto RESTART;
    cls->count++;
    if (cls->count + leavebehind >= nconsumers) {
      clauses.start[res] = 0;
      assert (!worker->dead);
      worker->dead = cls;
      assert (clauses.first <= res);
      clauses.first++;
      if (clauses.num > 10000 &&
	  clauses.first > clauses.num/(nconsumers + 1)) gcls ();
    }
    if (cls->wid == wid) goto RESTART;
    len = lencls (cls->lits);
    if (len + 1 >= worker->szcls) {
      newsize = 2*(len + 1);
      worker->cls = resize (0, worker->cls, 
                            worker->szcls * sizeof (int),
                            newsize * sizeof (int));
      worker->szcls = newsize;
    }
    memcpy (worker->cls, cls->lits, (len+1) * sizeof (int));
    *cptr = worker->cls;
    *glueptr = cls->glue;
    msg (wid, 3,
      "consuming glue %d length %d clause %d",
      *glueptr, len, res);
  }
  if (pthread_mutex_unlock (&clsmutex))
    warn ("failed to unlock 'cls' mutex in 'consumecls'");
}

static void consumedcls (void * voidptr, int consumed) {
  Worker * worker = voidptr;
  int wid = worker - workers;
  worker->stats.cls.consumed += consumed;
  worker->stats.consumed += consumed;
  msg (wid, 3, "consuming %d clause", consumed);
}

static int * lockrepr (void * voidptr) {
  Worker * worker = voidptr;
  int wid = worker - workers;
  if (pthread_mutex_lock (&reprmutex))
    warn ("failed to lock 'repr' mutex");
  msg (wid, 3, "starting equivalences synchronization");
  syncs.eqs++;
  return repr;
}

static void unlockrepr (void * voidptr, int consumed, int produced) {
  Worker * worker = voidptr;
  int wid = worker - workers;
  msg (wid, 3, 
       "finished equivalences synchronization: %d consumed, %d produced",
       consumed, produced);
  worker->stats.eqs.consumed += consumed;
  worker->stats.eqs.produced += produced;
  worker->stats.consumed += consumed;
  worker->stats.produced += produced;
  eqs += produced;
  assert (eqs < nvars);
  if (pthread_mutex_unlock (&reprmutex))
    warn ("failed to unlock 'repr' mutex");
}

static void msglock (void * voidptr) {
  (void) voidptr;
  pthread_mutex_lock (&msgmutex);
}

static void msgunlock (void * voidptr) {
  (void) voidptr;
  pthread_mutex_unlock (&msgmutex);
}

static void * work (void * voidptr) {
  Worker * worker = voidptr;
  int wid = worker - workers;
  LGL * lgl = worker->lgl;
  assert (0 <= wid && wid < nworkers);
  msg (wid, 1, "running");
  assert (workers <= worker && worker < workers + nworkers);
  worker->res = lglsat (lgl);
  msg (wid, 1, "result %d", worker->res);
  if (!worker->res) return 0;
  if (pthread_mutex_lock (&donemutex))
    warn ("failed to lock 'done' mutex in worker");
  done = 1;
  if (pthread_mutex_unlock (&donemutex)) 
    warn ("failed to unlock 'done' mutex in worker");
  msg (wid, 2, "%d decisions, %d conflicts, %.0f props, %.1f MB",
       lglgetdecs (lgl), lglgetconfs (lgl), lglgetprops (lgl), lglmb (lgl));
  if (verbose >= 2) {
    if (pthread_mutex_lock (&fixedmutex))
      warn ("failed to lock 'fixed' in work");
    msg (wid, 2, "consumed %d units %.0f%%, produced %d units %.0f%%",
	 worker->stats.units.consumed, 
	 percent (worker->stats.units.consumed, nfixed),
	 worker->stats.units.produced, 
	 percent (worker->stats.units.produced, nfixed));
    if (pthread_mutex_unlock (&fixedmutex))
      warn ("failed to unlock 'fixed' in work");
  }
  return worker;
}

static int64_t getsystemtotalmem (int explain) {
  long long res;
  FILE * p = popen ("grep MemTotal /proc/meminfo", "r");
  if (p && fscanf (p, "MemTotal: %lld kB", &res) == 1) {
    if (explain)
      msg (-1, 0, "%lld KB total memory according to /proc/meminfo", res);
    res <<= 10;
  } else {
    res = ((long long) MAXGB) << 30;;
    if (explain) 
      msg (-1, 0, "assuming compiled in memory size of %d GB", MAXGB);
  }
  if (p) pclose (p);
  return (int64_t) res;
}

static int getsystemcores (int explain) {
  int syscores, coreids, physids, procpuinfocores;
  int usesyscores, useprocpuinfo, amd, intel, res;
  FILE * p;

  syscores = sysconf (_SC_NPROCESSORS_ONLN);
  if (explain) {
    if (syscores > 0)
      msg (-1, 1, "'sysconf' reports %d processors online", syscores);
    else
      msg (-1, 1, "'sysconf' fails to determine number of online processors");
  }

  p = popen ("grep '^core id' /proc/cpuinfo 2>/dev/null|sort|uniq|wc -l", "r");
  if (p) {
    if (fscanf (p, "%d", &coreids) != 1) coreids = 0;
    if (explain) {
      if (coreids > 0) 
	msg (-1, 1, "found %d unique core ids in '/proc/cpuinfo'", coreids);
      else
	msg (-1, 1, "failed to extract core ids from '/proc/cpuinfo'");
    }
    pclose (p);
  } else coreids = 0;

  p = popen (
        "grep '^physical id' /proc/cpuinfo 2>/dev/null|sort|uniq|wc -l", "r");
  if (p) {
    if (fscanf (p, "%d", &physids) != 1) physids = 0;
    if (explain) {
      if (physids > 0) 
	msg (-1, 1, "found %d unique physical ids in '/proc/cpuinfo'", 
            physids);
      else
	msg (-1, 1, "failed to extract physical ids from '/proc/cpuinfo'");
    }
    pclose (p);
  } else physids = 0;

  if (coreids > 0 && physids > 0 && 
      (procpuinfocores = coreids * physids) > 0) {
    if (explain)
      msg (-1, 1, 
           "%d cores = %d core times %d physical ids in '/proc/cpuinfo'",
           procpuinfocores, coreids, physids);
  } else procpuinfocores = 0;

  usesyscores = useprocpuinfo = 0;

  if (procpuinfocores > 0 && procpuinfocores == syscores) {
    if (explain) msg (-1, 1, "'sysconf' and '/proc/cpuinfo' results match");
    usesyscores = 1;
  } else if (procpuinfocores > 0 && syscores <= 0) {
    if (explain) msg (-1, 1, "only '/proc/cpuinfo' result valid");
    useprocpuinfo = 1;
  } else if (procpuinfocores <= 0 && syscores > 0) {
    if (explain) msg (-1, 1, "only 'sysconf' result valid");
    usesyscores = 1;
  } else {
    intel = !system ("grep vendor /proc/cpuinfo 2>/dev/null|grep -q Intel");
    if (intel && explain) 
      msg (-1, 1, "found Intel as vendor in '/proc/cpuinfo'");
    amd = !system ("grep vendor /proc/cpuinfo 2>/dev/null|grep -q AMD");
    if (amd && explain) 
      msg (-1, 1, "found AMD as vendor in '/proc/cpuinfo'");
    assert (syscores > 0);
    assert (procpuinfocores > 0);
    assert (syscores != procpuinfocores);
    if (amd) {
      if (explain) msg (-1, 1, "trusting 'sysconf' on AMD");
      usesyscores = 1;
    } else if (intel) {
      if (explain) {
	msg (-1, 1, 
	     "'sysconf' result off by a factor of %f on Intel", 
	     syscores / (double) procpuinfocores);
	msg (-1, 1, "trusting '/proc/cpuinfo' on Intel");
      }
      useprocpuinfo = 1;
    }  else {
      if (explain)
	msg (-1, 1, "trusting 'sysconf' on unknown vendor machine");
      usesyscores = 1;
    }
  } 
  
  if (useprocpuinfo) {
    if (explain) 
      msg (-1, 0, 
        "assuming cores = core * physical ids in '/proc/cpuinfo' = %d",
        procpuinfocores);
    res = procpuinfocores;
  } else if (usesyscores) {
    if (explain) 
      msg (-1, 0,
           "assuming cores = number of processors reported by 'sysconf' = %d",
           syscores);
    res = syscores;
  } else {
    if (explain) 
      msg (-1, 0, "using compiled in default value of %d workers", NWORKERS);
    res = NWORKERS;
  }

  return res;
}

static int cmproduced (const void * p, const void * q) {
  Worker * u = *(Worker**) p;
  Worker * v = *(Worker**) q;
  int res = v->stats.produced - u->stats.produced;
  if (res) return res;
  return u - v;
}

static int cmpconsumed (const void * p, const void * q) {
  Worker * u = *(Worker**) p;
  Worker * v = *(Worker**) q;
  int res = v->stats.consumed - u->stats.consumed;
  if (res) return res;
  return u - v;
}

static int parsenbcoreenv (void) {
  const char * str = getenv ("NBCORE");
  if (!str) return 0;
  if (!isposnum (str)) 
    die ("invalid value '%s' for environment variable NBCORE", str);
  return atoi (str);
}

static void setopt (int i, LGL * lgl, const char * opt, int val) {
  int oldval, newval;
  if (!lglhasopt (lgl, opt))
    msg (i, 1, "option '%s' does not exist", opt);
  else if ((oldval = lglgetopt (lgl, opt)) == val)
    msg (i, 1, "option '%s' already set to '%d'", opt, val);
  else {
   lglsetopt (lgl, opt, val);
   newval = lglgetopt (lgl, opt);
   if (newval != val)
     msg (i, 1, "option '%s' set to '%d' (but requested %d)",
       opt, newval, val);
   else
     msg (i, 1, "option '%s' set to '%d'", opt, val);
  }
}

static void setopts (LGL * lgl, int i) {
  Worker * w = workers + i;
  int j;
  w->lgl = lgl;
  lglsetid (lgl, i, nworkers);
  lglsetime (lgl, getime);
  setopt (i, lgl, "verbose", verbose);
#ifndef NLGLOG
  if (!verbose)
  setopt (i, lgl, "log", loglevel);
#endif
  setopt (i, lgl, "seed", i);
  setopt (i, lgl, "classify", 0);

  if (i && (locs >= 2 || (locs && (i > 7 || (i & 1))))) {
    setopt (i, lgl, "plain", locs == 2 || (i & 3) == 1);
    setopt (i, lgl, "locs", -1);
    setopt (i, lgl, "locsrtc", 1);
    setopt (i, lgl, "locswait", 0);
    setopt (i, lgl, "locsclim", (1<<24));
  } else {
    j = locs ? i/2 : i;
    switch (j % 13) {
      case  0: default: break;
      case  1: setopt (i, lgl, "plain", 1),
               setopt (i, lgl, "decompose", 1); break;
      case  2: setopt (i, lgl, "restartint", 1000); break;
      case  3: setopt (i, lgl, "elmresched", 7); break;
      case  4: setopt (i, lgl, "scincincmin", 250); break;
      case  5: setopt (i, lgl, "block", 0),
               setopt (i, lgl, "cce", 0); break;
      case  6: setopt (i, lgl, "scincinc", 50); break;
      case  7: setopt (i, lgl, "phase", -1); break;
      case  8: setopt (i, lgl, "phase", 1); break;
      case  9: setopt (i, lgl, "sweeprtc", 1); break;
      case 10: setopt (i, lgl, "restartint", 100); break;
      case 11: setopt (i, lgl, "reduceinit", 10000);
               setopt (i, lgl, "reducefixed", 1); break;
      case 12: setopt (i, lgl, "restartint", 4); break;
    }
  }
  lglseterm (lgl, term, w);
  lglsetmsglock (lgl, msglock, msgunlock, w);
  if (!nounits) {
    lglsetproduceunit (lgl, produceunit, w);
    lglsetconsumeunits (lgl, consumeunits, w);
    lglsetconsumedunits (lgl, consumedunits, w);
  }
  if (!nocls) {
    lglsetproducecls (lgl, producecls, w);
    lglsetconsumecls (lgl, consumecls, w);
    lglsetconsumedcls (lgl, consumedcls, w);
  }
  if (!noeqs) {
    lglsetlockeq (lgl, lockrepr, w);
    lglsetunlockeq (lgl, unlockrepr, w);
  }
  msg (i, 2, "initialized");
}

static long long bytes2mbll (int64_t bytes) {
  return (bytes + (1ll<<20) - 1) >> 20;
}

static long long bytes2gbll (int64_t bytes) {
  return (bytes + (1ll<<30) - 1) >> 30;
}

static int cloneworker (int i) {
  assert (0 < i && i < nworkers);
  assert (!workers[i].lgl);
  msg (-1, 0, "trying to clone worker %d from worker 0", i);
  msg (-1, 0, 
    "prediction: %lld MB to be cloned + allocated %lld MB = %lld MB",
    bytes2mbll (lglbytes (workers[0].lgl)),
    bytes2mbll (mem.current),
    bytes2mbll (lglbytes (workers[0].lgl) + mem.current));

  if (lglbytes (workers[0].lgl) + mem.current >= softmemlimit) {
    msg (-1, 0,
      "will not clone worker %d since soft memory limit %lld MB would be hit",
     i, bytes2mbll (softmemlimit));
    return 0;
  }
  workers[i].lgl = lglclone (workers[0].lgl);
  setopts (workers[i].lgl, i);
  msg (-1, 0,
    "%lld MB total allocated memory after cloning worker %d from worker 0",
    bytes2mbll (mem.current), i);
  return 1;
}

int main (int argc, char ** argv) {
  int i, res, clin, lit, val, id, nbcore, witness = 1, tobecloned, tobestarted;
  Worker * w, * winner, *maxconsumer, * maxproducer, ** sorted, *earlyworker;
  int sumconsumed, sumconsumedunits, sumconsumedcls, sumconsumedeqs;
  const char * errstr, * arg;
  size_t bytes;
  char * cmd;
  start = currentime ();
  clin = 0;
  for (i = 1; i < argc; i++) {
    size_t totalmem = getsystemtotalmem (0);
    if (!strcmp (argv[i], "-h")) {
      printf (
"usage: plingeling [ <option> ... ] [ <dimacs>[.gz|.bz2|.7z|.zip|.lzma] [<threads>] ]\n"
"\n"
"where <option> is one of the following:\n"
"\n"
"  -h         print this command line option summary\n"
"  -v         increase verbose level\n"
#ifndef NLGLOG
"  -l         increase logging level\n"
#endif
"  -n         do not print solution / witness\n"
#ifndef NLGLYALSAT
"\n"
"  --force-locs    force (mainy) local search\n"
#endif
"\n"
"  -t <num>   number of worker threads (default %d on this machine)\n"
"  -m <num>   maximal memory in MB (default %lld MB on this machine)\n"
"  -g <num>   maximal memory in GB (default %lld GB on this machine)\n"
"  -l <num>   limit on number of workers before collecting shared clause\n"
"\n"
"  -p         plain portfolio, no sharing, e.g. implies the following:\n"
"\n"
"                --do-not-share-units\n"
"                --do-not-share-clauses\n"
"                --do-not-share-equivalences\n",
getsystemcores (0), bytes2mbll (totalmem), bytes2gbll (totalmem));
      exit (0);
    }
    if (!strcmp (argv[i], "-v")) verbose++;
#ifndef NLGLOG
    else if (!strcmp (argv[i], "-l")) loglevel++;
#endif
    else if (!strcmp (argv[i], "-p")) plain = 1;
    else if (!strcmp (argv[i], "-n")) witness = 0;
#ifndef NLGLYALSAT
    else if (!strcmp (argv[i], "--force-locs")) forcelocs++;
#endif
    else if (!strcmp (argv[i], "--do-not-share-units")) nounits = 1;
    else if (!strcmp (argv[i], "--do-not-share-clauses")) nocls = 1;
    else if (!strcmp (argv[i], "--do-not-share-equivalences")) noeqs = 1;
    else if (!strcmp (argv[i], "-t")) {
      if (nworkers) die ("multiple '-t' options");
      if (nworkers2) die ("number of threads and '-t' option given");
      if (i + 1 == argc) die ("argument to '-t' missing");
      if (!isposnum (arg = argv[++i]) || (nworkers = atoi (arg)) <= 0)
	die ("invalid argument '%s' to '-t'", arg);
    } else if (!strcmp (argv[i], "-m")) {
      if (memlimit) die ("multiple '-m' or '-g' options");
      if (i + 1 == argc) die ("argument to '-m' missing");
      if (!isposnum (arg = argv[++i]) || (memlimit = (atoll (arg)<<20)) <= 0)
	die ("invalid argument '%s' to '-m'", arg);
    } else if (!strcmp (argv[i], "-g")) {
      if (memlimit) die ("multiple '-g' or '-m' options");
      if (i + 1 == argc) die ("argument to '-g' missing");
      if (!isposnum (arg = argv[++i]) || (memlimit = (atoll (arg)<<30)) <= 0)
	die ("invalid argument '%s' to '-g'", arg);
    } else if (!strcmp (argv[i], "-l")) {
      if (gclimset) die ("multiple '-l' options");
      if (i + 1 == argc) die ("argument to '-l' missing");
      if (!isposnum (argv[++i]))
	die ("invalid argument '%s' to '-l'", argv[i]);
      gclim = atoi (argv[i]);
      gclimset = 1;
    } else if (argv[i][0] == '-') 
      die ("invalid option '%s' (try '-h')", argv[i]);
    else if (!name && isposnum (argv[i]))
      die ("<dimacs> file name can not be a positive number '%s'", argv[i]);
    else if (name && nworkers2)
      die ("too many arguments (including <dimacs> and <threads>)");
    else if (name && !isposnum (argv[i]))
      die ("expected positive number of threads but got '%s'", argv[i]);
    else if (name) {
      if ((nworkers2 = atoi (argv[i])) <= 0)
	die ("invalid number of threads '%s'", argv[i]);
    } else name = argv[i];
  }
  if (nworkers2) assert (!nworkers), nworkers = nworkers2;
  lglbnr ("Plingeling Parallel SAT Solver", "c ", stdout);
  fflush (stdout);
  if (verbose) printf ("c\n");
  nbcore = parsenbcoreenv ();
  if (nworkers) {
    msg (-1, 1, 
	 "command line option '-t %d' overwrites system default %d",
	 nworkers, getsystemcores (0));
    if (nbcore)
      msg (-1, 1, 
           "and also overwrites environment variable NBCORE=%d",
	   nbcore);
  } else if (nbcore) {
    msg (-1, 1, 
	 "environment variable NBCORE=%d overwrites system default %d",
	 nbcore, getsystemcores (0));
    nworkers = nbcore;
  } else {
    msg (-1, 1,
      "no explicit specification of number of workers");
      nworkers = getsystemcores (1);
  }
  msg (-1, 0, "USING %d WORKER THREADS", nworkers);
  if (memlimit) {
    msg (-1, 1,
      "memory limit %lld MB ('-g %lld') overwrites system default %lld MB",
      bytes2mbll (memlimit), bytes2gbll (memlimit),
      bytes2mbll (getsystemtotalmem (0)));
  } else {
    memlimit = getsystemtotalmem (1);
    msg (-1, 1, "memory limit set to system default of %lld MB total memory",
         bytes2mbll (memlimit));
  }
  softmemlimit = (memlimit + 2)/3;
  msg (-1, 0, "soft memory limit set to %lld MB", bytes2mbll (softmemlimit));
  if (gclimset)
    msg (-1, 0, "garbage collection limit %d ('-l %d')", gclim, gclim);
  else msg (-1, 0, "default garbage collection limit %d", gclim);
  if (plain) {
    nounits = nocls = noeqs = 1;
    msg (-1, 0, "not sharing anything in plain portolio mode ('-p')");
  } else {
    if (noeqs) msg (-1, 0, "not sharing units ('--do-not-share-units')");
    else msg (-1, 0, "sharing of units enabled");
    if (nocls) msg (-1, 0, "not sharing clauses ('--do-not-share-clauses')");
    else msg (-1, 0, "sharing of clauses enabled");
    if (noeqs)
      msg (-1, 0,
        "not sharing equivalences ('--do-not-share-equivalences')");
    else msg (-1, 0, "sharing of equivalences enabled");
  }
  NEW (workers, nworkers);
  workers[0].lgl = lglminit (0, alloc, resize, dealloc);
  lglsetopt (workers[0].lgl, "druplig", 0);
  setopt (0, workers[0].lgl, "bca", 0);
  if (!verbose) setopt (0, workers[0].lgl, "trep", 1);
  if (!verbose) setopt (0, workers[0].lgl, "profile", 0);
  if (verbose) lglopts (workers[0].lgl, "c 0 ", 0);
  setsighandlers ();
  if (name) { 
    if (strlen (name) >= 3 && !strcmp (name + strlen(name) - 3, ".gz")) {
      bytes = strlen (name) + 30;
      cmd = alloc (0, bytes);
      sprintf (cmd, "gunzip -c %s", name);
      file = popen (cmd, "r");
      dealloc (0, cmd, bytes);
      clin = 4;
    } else if (strlen (name) >= 4 &&
               !strcmp (name + strlen(name) - 4, ".zip")) {
      bytes = strlen (name) + 30;
      cmd = alloc (0, bytes);
      sprintf (cmd, "unzip -p %s", name);
      file = popen (cmd, "r");
      dealloc (0, cmd, bytes);
      clin = 4;
    } else if (strlen (name) >= 5 &&
               !strcmp (name + strlen(name) - 5, ".lzma")) {
      bytes = strlen (name) + 30;
      cmd = alloc (0, bytes);
      sprintf (cmd, "lzcat %s", name);
      file = popen (cmd, "r");
      dealloc (0, cmd, bytes);
      clin = 4;
    } else if (strlen (name) >= 4 &&
               !strcmp (name + strlen(name) - 4, ".bz2")) {
      bytes = strlen (name) + 30;
      cmd = alloc (0, bytes);
      sprintf (cmd, "bzcat %s", name);
      file = popen (cmd, "r");
      dealloc (0, cmd, bytes);
      clin = 4;
    } else if (strlen (name) >= 3 && 
               !strcmp (name + strlen (name) - 3, ".7z")) {
      bytes = strlen (name) + 40;
      cmd = alloc (0, bytes);
      sprintf (cmd, "7z x -so %s 2>/dev/null", name);
      file = popen (cmd, "r");
      dealloc (0, cmd, bytes);
      clin = 4;
    } else {
      file = fopen (name, "r");
      clin = 1;
    }
    if (!file) die ("can not read %s", name);
  } else file = stdin, name = "<stdin>";
  msg (-1, 0, "parsing %s", name);
  errstr = parse ();
  if (errstr) die ("parse error: %s", errstr);
  if (clin == 1) fclose (file);
  if (clin == 2) pclose (file);
#ifdef NLGLYALSAT
  locs = 0;
#else
  if (forcelocs) locs = forcelocs;
#endif
  setopts (workers[0].lgl, 0);
  if (locs >= 2) nconsumers = 1;
  else if (locs == 1) {
    nconsumers = 1;
    for (i = 1; i < nworkers; i++)
      if (i < 8 && !(i & 1)) nconsumers++;
  } else nconsumers = nworkers;
  msg (-1, 1,
    "assuming %d consumers out of %d workers",
    nconsumers, nworkers);
  leavebehind = nconsumers / 4;
  msg (-1, 1,
    "will leave behind %d consumers out of %d",
    leavebehind, nconsumers);
  earlyworker = (nworkers > 1 && cloneworker (1)) ? workers + 1 : 0;
  if (earlyworker) {
    assert (earlyworker->lgl);
    if (pthread_create (&earlyworker->thread, 0, work, earlyworker))
      die ("failed to create additional early worker thread 1");
    msg (-1, 2, "started additional early worker 1");
  }
  if (!locs) {
    msg (-1, 0, "simplifying original formula with worker 0");
    lglsetopt (workers[0].lgl, "clim", 0);
    (void) work (workers + 0);
    lglsetopt (workers[0].lgl, "clim", -1);
  }
  res = workers[0].res;
  if (res) {
    msg (-1, 1,
      "simplification of first worker 0 produced %d", res);
    if (earlyworker) {
      if (pthread_join (earlyworker->thread, 0))
	die ("failed to join early worker thread 1");
      msg (-1, 2, "joined early worker 1");
    }
  } else {
    if ((tobecloned = (nworkers - 2 + !earlyworker)) > 0) {
      msg (-1, 0, "trying to clone %d workers", tobecloned);
      for (i = 1; i < nworkers; i++)
	if (earlyworker != workers + i && !cloneworker (i))
	  break;
    }
    tobestarted = 0;
    for (i = 0; i < nworkers; i++)
      if (workers + i != earlyworker)
	tobestarted++;
    assert (tobestarted > 0);
    msg (-1, 2, "starting %d worker threads", tobestarted);
    for (i = 0; i < nworkers; i++) {
      w = workers + i;
      if (w == earlyworker) continue;
      if (!w->lgl) continue;
      if (pthread_create (&w->thread, 0, work, w))
	die ("failed to create worker thread %d", i);
      msg (-1, 2, "started worker %d", i);
    }
    msg (-1, 2, "joining %d workers", tobestarted + 1 - !earlyworker);
    for (i = 0; i < nworkers; i++) {
      w = workers + i;
      if (!w->lgl) continue;
      if (pthread_join (w->thread, 0))
	die ("failed to join worker thread %d", i);
      msg (-1, 2, "joined worker %d", i);
    }
  }
  maxproducer = maxconsumer = winner = 0;
  for (i = 0; i < nworkers; i++) {
    w = workers + i;
    if (!w->lgl) continue;
    if (w->res) {
      if (!res) {
	res = w->res;
	winner = w;
	msg (-1, 0, "worker %d is the WINNER with result %d", i, res);
      } else if (res != w->res) die ("result discrepancy");
    }
    if (!maxconsumer || w->stats.consumed > maxconsumer->stats.consumed)
      maxconsumer = w;
    if (!maxproducer || w->stats.produced > maxproducer->stats.produced)
      maxproducer = w;
  }
  NEW (sorted, nworkers);
  for (i = 0; i < nworkers; i++) sorted[i] = workers + i;
  printf ("c\n");
  assert (maxproducer);
  qsort (sorted, nworkers, sizeof *sorted, cmproduced);
  for (i = 0; i < nworkers; i++) {
    w = sorted[i];
    if (!w->lgl) continue;
    id = w - workers;
    printf (
      "c %2d %s %7d %3.0f%% = %7d units %3.0f%% + %7d cls %3.0f%% + %7d eqs %3.0f%%\n",
       id, (w == maxproducer ? "PROD" : "prod"),
       w->stats.produced,
         percent (w->stats.produced, units + eqs + clauses.added),
       w->stats.units.produced, percent (w->stats.units.produced, units),
       w->stats.cls.produced, percent (w->stats.cls.produced, clauses.added),
       w->stats.eqs.produced, percent (w->stats.eqs.produced, eqs));
  }
  fputs ("c ", stdout);
  for (i = 0; i < 79; i++) fputc ('-', stdout);
  fputc ('\n', stdout);
  printf (
    "c    prod %7lld 100%% = %7d units 100%% + %7lld cls 100%% + %7d eqs 100%%\n",
    (long long) units + eqs + clauses.added, units, (long long) clauses.added, eqs);
  printf ("c\n");
  assert (maxconsumer);
  qsort (sorted, nworkers, sizeof *sorted, cmpconsumed);
  sumconsumed = sumconsumedunits = sumconsumedcls = sumconsumedeqs =0;
  for (i = 0; i < nworkers; i++) {
    w = sorted[i];
    if (!w->lgl) continue;
    id = w - workers;
    sumconsumed += w->stats.consumed;
    sumconsumedeqs += w->stats.eqs.consumed;
    sumconsumedcls += w->stats.cls.consumed;
    sumconsumedunits += w->stats.units.consumed;
    printf (
      "c %2d %s %7d %3.0f%% = %7d units %3.0f%% + %7d cls %3.0f%% + %7d eqs %3.0f%%\n",
      id, (w == maxconsumer ? "CONS" : "cons"),
      w->stats.consumed, percent (w->stats.consumed, units + eqs + clauses.added),
      w->stats.units.consumed, percent (w->stats.units.consumed, units),
      w->stats.cls.consumed, percent (w->stats.cls.consumed, clauses.added),
      w->stats.eqs.consumed, percent (w->stats.eqs.consumed, eqs));
  }
  fputs ("c ", stdout);
  for (i = 0; i < 79; i++) fputc ('-', stdout);
  fputc ('\n', stdout);
  printf (
    "c    cons %7d %3.0f%% = %7d units %3.0f%% + %7d cls %3.0f%% + %7d eqs %3.0f%%\n",
    sumconsumed,  percent (sumconsumed, units + eqs + clauses.added),
    sumconsumedunits, percent (sumconsumedunits, units),
    sumconsumedcls, percent (sumconsumedcls, clauses.added),
    sumconsumedeqs, percent (sumconsumedeqs, eqs));
  DEL (sorted, nworkers);
  fflush (stdout);
  if (!res) res = globalres;
  if (!res) die ("no result by any worker");
  assert (res);
  msg (-1, 2, "copying assignment");
  if (winner && res == 10) {
    for (i = 1; i <= nvars; i++) {
      val = lglderef (winner->lgl, i);
      if (vals[i]) assert (val == vals[i]);
      vals[i] = val;
    }
  }
  resetsighandlers ();

  if (res == 10) {
    printf ("s SATISFIABLE\n");
    if (witness) {
      fflush (stdout);
      if (nvars) {
	printf ("v");
	for (i = 1; i <= nvars; i++) {
	  if (!(i & 7)) fputs ("\nv", stdout);
	  lit = vals[i] < 0 ? -i : i;
	  printf (" %d", lit);
	}
	printf ("\n");
      }
      printf ("v 0\n");
    }
  } else if (res == 20) {
    printf ("s UNSATISFIABLE\n");
  } else printf ("c s UNKNOWN\n");
  fflush (stdout);

  if (verbose) {
    for (i = 0; i < nworkers; i++) {
      if (!workers[i].lgl) continue;
      printf ("c\nc ------------[worker %d statistics]------------ \nc\n", i);
      lglstats (workers[i].lgl);
    }
    printf ("c\nc -------------[overall statistics]------------- \nc\n");
  } else printf ("c\n");
  stats ();

  if (verbose >= 2) printf ("c\n");
  msg (-1, 2, "releasing %d workers", nworkers);
  for (i = 0; i < nworkers; i++) {
    w = workers + i;
    if (!w->lgl) continue;
    lglrelease (w->lgl);
    msg (-1, 2, "released worker %d", i);
    if (w->dead) deletecls (w->dead), w->dead = 0;
    DEL (w->cls, w->szcls);
  }
  DEL (workers, nworkers);
  DEL (fixed, nvars + 1);
  if (!noeqs) DEL (repr, nvars + 1);
  DEL (vals, nvars + 1);
  deleteallcls ();

  assert (getenv ("PLINGELINGLEAK") || !mem.current);

  return res;
}
