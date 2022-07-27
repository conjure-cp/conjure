/*-------------------------------------------------------------------------*/
/* Copyright 2010-2018 Armin Biere Johannes Kepler University Linz Austria */
/*-------------------------------------------------------------------------*/

#include "lglib.h"

#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <pthread.h>
#include <signal.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

/*------------------------------------------------------------------------*/
#if 0
#define LOG(ARGS) do { printf ("c %s\n", ARGS); fflush (stdout); } while (0)
#else
#define LOG(ARGS...) do  { } while (0)
#endif
/*------------------------------------------------------------------------*/

#define NCORES			8
#define CORES2WORKERS(C)	(C)
#define ACTIVEPERWORKER		8
#define WORKERS2ACTIVE(W)	(ACTIVEPERWORKER*(W))
#define CORES2ACTIVE(C)		WORKERS2ACTIVE(CORES2WORKERS(C))
#define MAXGB			12ll
#define MAXBYTES		(MAXGB<<30)
#define MINCLIM			1000
#define INITCLIM 		10000
#define MAXCLIM			100000
#define STOPLKHDRED		20
#define MAXSTOPLKH		128
#define FULLINT			10
#define ASYMMETRIC		1
#define FULLSIMP		4
#define FULLSEARCH		2
#define DEFBRANCHES		50
#define OPTIMIZE		10

/*------------------------------------------------------------------------*/

#define NEW(PTR,NUM) \
do { \
  size_t BYTES = (NUM) * sizeof *(PTR); \
  (PTR) = malloc (BYTES); \
  if (!(PTR)) { err ("out of memory"); exit (1); } \
  memset ((PTR), 0, BYTES); \
  incmem (BYTES); \
} while (0)

#define DEL(PTR,NUM) \
do { \
  size_t BYTES = (NUM) * sizeof *(PTR); \
  decmem (BYTES); \
  free (PTR); \
} while (0)

#define PUSH(STACK,ELEM) \
do { \
  if (size ## STACK == num ## STACK) { \
    size_t NEW_SIZE = size ## STACK; \
    size_t OLD_BYTES = NEW_SIZE * sizeof *STACK, NEW_BYTES; \
    if (NEW_SIZE) NEW_SIZE *= 2; else NEW_SIZE = 1; \
    NEW_BYTES = NEW_SIZE * sizeof *STACK; \
    decmem (OLD_BYTES); \
    STACK = realloc (STACK, NEW_BYTES); \
    if (!STACK) { err ("out of memory"); exit (1); } \
    incmem (NEW_BYTES); \
    size ## STACK = NEW_SIZE; \
  } \
  STACK[num ## STACK ++] = (ELEM); \
} while (0)

#define LL long long

/*------------------------------------------------------------------------*/

typedef enum State { 
  FREE = 0,
  READY = 1,
  SIMP = 2,
  LKHD = 3,
  SPLIT = 4,
  SEARCH = 5,
} State;

typedef struct Node {
  State state;
  int pos, lookahead, depth, res, simplified, consumed;
  int64_t id, decisions, conflicts, propagations;
  int * cube;
  LGL * lgl;
} Node;

typedef struct Leaf {
  int64_t id;
  struct Leaf * next, * prev;
  int * lits;
} Leaf;

typedef struct Parallel {
  int64_t decisions, conflicts, propagations;
  struct { int64_t leafs, units; } consumed, produced;
  int res, nunits, *units;
  pthread_t thread;
  LGL * lgl;
} Parallel;

typedef struct Job {
  int pos;
  State state;
  Node * node;
  void * (*fun)(void *);
  pthread_t thread;
  const char * name;
} Job;

typedef struct Lock {
  pthread_mutex_t mutex;
  int locked, waited;
} Lock;

/*------------------------------------------------------------------------*/

static int verbose, balance, showstats, nowitness, ncores, randswap, locslkhd;
static int reducecache, nosimp, forcesimp, forcelkhd = 1, nosearch, noparallel;
static int stoplkhd, stoplkhdint = 1, lkhdsuccessful, optimize = -1;
static int fullint = FULLINT, asymmetric = ASYMMETRIC, eager = 1;
static int splitsuccessful = 1, branches = -1, portfolio = 0;

static int clim, newclim, forcedclim, thisclim, initclim, maxclim, minclim;
static int nvars, nclauses;

static LGL * root;
static Node ** nodes;
static int numnodes, maxnumnodes, sizenodes;
static int rootconsumed;

static Parallel parallel;
static int nparallel;

static int maxactive, firstosplit, numtosplit, lastosplit;
static int maxworkers, maxworkers2, numworkers, maxnumworkers;

static Job ** jobs;
static int numjobs, sizejobs;
static struct { int64_t cnt, lkhd, split, simp, search; } js;

static int64_t * confstack;
static int numconfstack, sizeconfstack;

static const char * fname;
static FILE * file;
static int lineno;

size_t maxbytes, hardlimbytes, softlimbytes, splitlimbytes, currentbytes;
static int64_t ids, threads, conflicts, decisions, propagations;
static int64_t sumclims, inclims, declims, forcedclims, sumsimplified;
static struct { double epoch, simp, lkhd, split, search; } wct;
static int round, started, deleted, simplified, added;
static double * startimeptr, startime;
static struct { int64_t set, def; } opts;

struct { int num, max; int64_t count; Leaf * first, * last; } leafs;

static int done, stop;

static struct { unsigned z, w; } rng;

static struct { 
  Lock confs;
  Lock done;
  Lock leafs;
  Lock mem;
  Lock msg;
  Lock nodes;
  Lock opts;
  Lock parleafs;
  Lock parstats;
  Lock parunits;
  Lock simplified;
  Lock stats;
  Lock workers;
} lock;

static pthread_cond_t workerscond;

/*------------------------------------------------------------------------*/

static double currentime () {
  double res = 0;
  struct timeval tv;
  if (!gettimeofday (&tv, 0))
    res = 1e-6 * tv.tv_usec, res += tv.tv_sec;
  return res;
}

static double getime () { return currentime () - wct.epoch; }

static void warn (const char * fmt, ...) {
  va_list ap;
  fputs ("c *** warning *** ", stdout);
  va_start (ap, fmt);
  vprintf (fmt, ap);
  va_end (ap);
  fputc ('\n', stdout);
  fflush (stdout);
}

static void startimer (double * timptr) {
  assert (!started);
  startimeptr = timptr;
  startime = currentime ();
  started = 1;
}

static double deltatime (double start) {
  double res = currentime () - start;
  if (res < 0) res = -res;
  return res;
}

static double stoptimer () {
  double * ptr, res = deltatime (startime);
  assert (started);
  assert (startimeptr);
  started = 0;
  if ((ptr = startimeptr)) *ptr += res;
  startimeptr = 0;
  return res;
}

/*------------------------------------------------------------------------*/

static void lockgen (Lock * lock, const char * name) {
  if (pthread_mutex_lock (&lock->mutex))
    warn ("failed to lock '%s' mutex", name);
  assert (!lock->locked || lock->waited);
  lock->locked++;
}

static void unlockgen (Lock *lock, const char * name) {
  assert (lock->locked > 0 || lock->waited);
  lock->locked--;
  if (pthread_mutex_unlock (&lock->mutex))
    warn ("failed to lock '%s' mutex", name);
}

#define LOCK(NAME) \
  static void lock ## NAME () { \
    lockgen (&lock.NAME, # NAME); \
  }
#define UNLOCK(NAME) \
  static void unlock ## NAME () { \
    unlockgen (&lock.NAME, # NAME); \
  }

LOCK (confs)
LOCK (done)
LOCK (leafs)
LOCK (mem)
LOCK (msg)
LOCK (nodes)
LOCK (opts)
LOCK (parleafs)
LOCK (parstats)
LOCK (parunits)
LOCK (simplified)
LOCK (stats)
LOCK (workers)
UNLOCK (confs)
UNLOCK (done)
UNLOCK (leafs)
UNLOCK (mem)
UNLOCK (msg)
UNLOCK (nodes)
UNLOCK (opts)
UNLOCK (parleafs)
UNLOCK (parstats)
UNLOCK (parunits)
UNLOCK (simplified)
UNLOCK (stats)
UNLOCK (workers)

/*------------------------------------------------------------------------*/

static void err (const char * fmt, ...) {
  va_list ap;
  lockmsg ();
  fputs ("c *** ", stdout);
  va_start (ap, fmt);
  vprintf (fmt, ap);
  va_end (ap);
  fputc ('\n', stdout);
  fflush (stdout);
  unlockmsg ();
  exit (1);
}

static void smsg () {
  double t = getime (), m;
  lockmem ();
  m  = currentbytes/(double)(1<<20);
  unlockmem ();
  printf (
    "(%.1f %d %lld %d %d %.0f) ",
    t, round, (LL) ids, numnodes, clim, m);
}

static void msg (const char * fmt, ...) {
  va_list ap;
  lockmsg ();
  fputs ("c ", stdout);
  va_start (ap, fmt);
  vprintf (fmt, ap);
  va_end (ap);
  fputc ('\n', stdout);
  fflush (stdout);
  unlockmsg ();
}

static void vrb (const char * fmt, ...) {
  va_list ap;
  if (!verbose) return;
  lockmsg ();
  fputs ("c ", stdout);
  smsg ();
  va_start (ap, fmt);
  vprintf (fmt, ap);
  va_end (ap);
  fputc ('\n', stdout);
  fflush (stdout);
  unlockmsg ();
}

static void nmsg (Node * node, const char * fmt, ...) {
  va_list ap;
  if (!verbose) return;
  lockmsg ();
  printf ("c [%d %lld] ", node->depth, (LL) node->id);
  smsg ();
  va_start (ap, fmt);
  vprintf (fmt, ap);
  va_end (ap);
  fputc ('\n', stdout);
  fflush (stdout);
  unlockmsg ();
}

static void jmsg (Job * job, const char * msg) {
  Node * node = job->node;
  if (!verbose) return;
  lockmsg ();
  printf ("c [%d %lld] ", node->depth, (LL) node->id);
  smsg ();
  printf ("%s %s job %d\n", msg, job->name, job->pos);
  fflush (stdout);
  unlockmsg ();
}

static void mmsg (const char * msg, Node * node) {
  if (!verbose) return;
  lockmsg ();
  printf ("c ");
  smsg ();
  printf ("%s [%d %lld]\n", 
          msg, node->depth, (LL) node->id);
  fflush (stdout);
  unlockmsg ();
}

static void perr (const char * fmt, ...) {
  va_list ap;
  printf ("c *** parse error in '%s' at line %d: ", fname, lineno);
  va_start (ap, fmt);
  vprintf (fmt, ap);
  va_end (ap);
  fputc ('\n', stdout);
  fflush (stdout);
  exit (1);
}

static int skipnode (Node * node) {
  assert (node->state != FREE);
  if (lglinconsistent (node->lgl)) return 1;
  return 0;
}

static void incround () {
  round++;
  vrb ("");
  vrb ("=================== [ round %d ] ===================", round);
  vrb ("");
}

static void startphase (const char * phase) {
  if (!verbose) return;
  vrb ("");
  vrb ("------------------- [ %s %d ] -------------------", phase, round);
  vrb ("");
}

/*------------------------------------------------------------------------*/

static double avg (double a, double b) { return b > 0 ? a/b : 0.0; }
static double pcnt (double a, double b) { return avg (100.0 * a, b); }

/*------------------------------------------------------------------------*/

static void incmem (size_t bytes) {
  lockmem ();
  currentbytes += bytes;
  if (currentbytes > maxbytes) maxbytes = currentbytes;
  unlockmem ();
}

static void decmem (size_t bytes) {
  lockmem ();
  assert (currentbytes >= bytes);
  currentbytes -= bytes;
  unlockmem ();
}

static void * alloc (void * dummy, size_t bytes) {
  char * res;
  NEW (res, bytes);
  return res;
}

static void dealloc (void * dummy, void * void_ptr, size_t bytes) {
  char * char_ptr = void_ptr;
  DEL (char_ptr, bytes);
}

static void * resize (void * dummy, void * ptr, 
                      size_t old_bytes, size_t new_bytes) {
  lockmem ();
  assert (currentbytes >= old_bytes);
  currentbytes -= old_bytes;
  currentbytes += new_bytes;
  if (currentbytes > maxbytes) maxbytes = currentbytes;
  unlockmem ();
  return realloc (ptr, new_bytes);
}

static int64_t getotalmem (int explain) {
  long long res;
  FILE * p = popen ("grep MemTotal /proc/meminfo", "r");
  if (p && fscanf (p, "MemTotal: %lld kB", &res) == 1) {
    if (explain)
      msg ("%lld KB total memory according to '/proc/meminfo'", res);
    res <<= 10;
  } else {
    res = MAXGB << 30;
    if (explain) 
      msg ("assuming compiled in memory size of %d GB", MAXGB);
  }
  if (p) pclose (p);
  return (int64_t) res;
}

static int getcores (int explain) {
  int syscores, coreids, physids, procpuinfocores;
  int usesyscores, useprocpuinfo, amd, intel, res;
  FILE * p;

  syscores = sysconf (_SC_NPROCESSORS_ONLN);
  if (explain) {
    if (syscores > 0)
      msg ("'sysconf' reports %d processors online", syscores);
    else
      msg ("'sysconf' fails to determine number of online processors");
  }

  p = popen ("grep '^core id' /proc/cpuinfo 2>/dev/null|sort|uniq|wc -l", "r");
  if (p) {
    if (fscanf (p, "%d", &coreids) != 1) coreids = 0;
    if (explain) {
      if (coreids > 0) 
	msg ("found %d unique core ids in '/proc/cpuinfo'", coreids);
      else
	msg ("failed to extract core ids from '/proc/cpuinfo'");
    }
    pclose (p);
  } else coreids = 0;

  p = popen (
        "grep '^physical id' /proc/cpuinfo 2>/dev/null|sort|uniq|wc -l", "r");
  if (p) {
    if (fscanf (p, "%d", &physids) != 1) physids = 0;
    if (explain) {
      if (physids > 0) 
	msg ("found %d unique physical ids in '/proc/cpuinfo'", 
            physids);
      else
	msg ("failed to extract physical ids from '/proc/cpuinfo'");
    }
    pclose (p);
  } else physids = 0;

  if (coreids > 0 && physids > 0 && 
      (procpuinfocores = coreids * physids) > 0) {
    if (explain)
      msg ("%d cores = %d core times %d physical ids in '/proc/cpuinfo'",
           procpuinfocores, coreids, physids);
  } else procpuinfocores = 0;

  usesyscores = useprocpuinfo = 0;

  if (procpuinfocores > 0 && procpuinfocores == syscores) {
    if (explain) msg ("'sysconf' and '/proc/cpuinfo' results match");
    usesyscores = 1;
  } else if (procpuinfocores > 0 && syscores <= 0) {
    if (explain) msg ("only '/proc/cpuinfo' result valid");
    useprocpuinfo = 1;
  } else if (procpuinfocores <= 0 && syscores > 0) {
    if (explain) msg ("only 'sysconf' result valid");
    usesyscores = 1;
  } else if (procpuinfocores > 0 && syscores > 0) {
    intel = !system ("grep vendor /proc/cpuinfo 2>/dev/null|grep -q Intel");
    if (intel && explain) 
      msg ("found Intel as vendor in '/proc/cpuinfo'");
    amd = !system ("grep vendor /proc/cpuinfo 2>/dev/null|grep -q AMD");
    if (amd && explain) 
      msg ("found AMD as vendor in '/proc/cpuinfo'");
    assert (syscores > 0);
    assert (procpuinfocores > 0);
    assert (syscores != procpuinfocores);
    if (amd) {
      if (explain) msg ("trusting 'sysconf' on AMD");
      usesyscores = 1;
    } else if (intel) {
      if (explain) {
	msg ("'sysconf' result off by a factor of %f on Intel", 
	     syscores / (double) procpuinfocores);
	msg ("trusting 'sysconf' on Intel (assuming HyperThreading)");
      }
      usesyscores = 1;
    }  else {
      if (explain)
	msg ("trusting 'sysconf' on unknown vendor machine");
      usesyscores = 1;
    }
  } 
  
  if (useprocpuinfo) {
    if (explain) 
      msg ("assuming cores = core * physical ids in '/proc/cpuinfo' = %d",
           procpuinfocores);
    res = procpuinfocores;
  } else if (usesyscores) {
    if (explain) 
      msg (
       "assuming cores = number of processors reported by 'sysconf' = %d",
       syscores);
    res = syscores;
  } else {
    if (explain) 
      msg ("using compiled in default value of assumed %d cores", NCORES);
    res = NCORES;
  }

  return res;
}

static void usage () {
  int64_t b = getotalmem (0);
  long long m = ((b+(1<<20)-1) >> 20), g = ((b+(1<<30)-1) >> 30); 
  int c = getcores (0);
  printf (
"usage: treengeling [<option> ...] [<file> [<workers>]]\n"
"\n"
"where <option> is one of the following\n"
"\n"
"  -h             print option summary\n"
"  --version      print version and exit\n"
"  -v             increase verbose level\n"
"  -S             print statistics for each solver instance too\n"
"  -n             do not print satisfying assignments\n"
"\n"             
"  -t <workers>   maximum number actual worker threads (system default %d)\n"
"  -a <nodes>     maximum number active nodes (system default %d)\n"
"\n"             
"  -m <mb>        assumed memory in mega bytes (system default %lld MB)\n"
"  -g <gb>        assumed memory in giga bytes (system default %lld GB)\n"
"\n"             
"  -r <posnum>    randomize splits by swapping <posnum> nodes\n"
"  -s <seed>      unsigned 64 bit seed for randomizing splits (default 0)\n"
"\n"             
"  -b <branches>  percentage of nodes split (default %d%%)\n"
"\n"             
"  --balance      split larger nodes first\n"
"  --symmetric    symmetric splitting%s\n"
"  --asymmetric   asymmetric splitting%s\n"
"  --eager        eager splitting by forced reduction of limit (default)\n"
"  --lazy         disable eager splitting (opposite of '--eager')\n"
"  --portfolio    use portfolio style option fuzzing (default off)\n"
"\n"             
" --min=<lim>     minimum conflict limit per search (compiled default %d)\n"
" --init=<lim>    initial conflict limit per search (compiled default %d)\n"
" --max=<lim>     maximum conflict limit per search (compiled default %d)\n"
"\n"             
"  --reduce       reduce learned clause cache for all right branches\n"
"  --force-simp   force simplification even after light simplification\n"
"  --force-lkhd   force full look-ahead every time\n"
"  --no-simp      do not explicitly simplify in each round\n"
"  --no-search    do not even search in each round\n"
"  --no-parallel  disable additional parallel solver instance\n"
"  --no-full      no full rounds every %d rounds\n"
"  -f <fullint>   full round interval (default %d)\n"
"\n"
"and the <file> is a DIMACS file.  If the name of the file has a '.gz'\n"
"respectively '.bz2' suffix, it is assumed to be a file compressed with\n"
"'gzip' respectively 'bzip2'.  In this case the parser will open a pipe\n"
"and execute 'gunzip' respectively 'bzcat'.\n",
  CORES2WORKERS (c),
  CORES2ACTIVE (c),
  m,
  g, 
  DEFBRANCHES,
  ASYMMETRIC ? "" : " (default)",
  ASYMMETRIC ? " (default)" : "",
  MINCLIM,
  INITCLIM,
  MAXCLIM,
  FULLINT,
  FULLINT);
  fflush (stdout);
  exit (0);
}

static void version () { printf ("%s\n", lglversion ()); exit (0); }

static int next () {
  int res = getc (file);
  if (res == '\n') lineno++;
  return res;
}

static int ws (int ch) {
  return ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r';
}

static void parse (LGL * lgl) {
  int sclauses, ch, sign, lit, last, nlits;
  lineno = 1;
HEADER:
  if ((ch = next ()) == EOF) perr ("unexpected end-of-file before header");
  if (ch == 'c') {
    while ((ch = next ()) != '\n')
      if (ch == EOF) perr ("unexpected end-of-file in header comment");
    goto HEADER;
  }
  if (ch != 'p') perr ("unexpected character 0x%02x in header", ch);
  if ((fscanf (file, " cnf %d %d", &nvars, &sclauses)) != 2 ||
      nvars < 0 || sclauses < 0)
    perr ("invalid header");
  msg ("found 'p cnf %d %d' header", nvars, sclauses);
  nlits = last = 0;
BODY:
  ch = next ();
  if (ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r') goto BODY;
  if (ch == EOF) {
    if (last) perr ("zero missing after %d in last clause", last);
    if (nclauses < sclauses) perr ("%d clauses missing", sclauses - nclauses);
    msg ("parsed %d literals in %d clauses in %.2f seconds",
      nlits, nclauses, getime ());
    return;
  }
  if (ch == 'c') {
    while ((ch = next ()) != '\n')
      if (ch == EOF) perr ("unexpected end-of-file in body comment");
    goto BODY;
  }
  sign = 1;
  if (ch == '-') {
    if (!isdigit (ch = next ())) perr ("expected digit after '-'");
    sign = -1;
  } else if (!isdigit (ch)) perr ("expected digit or '-'");
  lit = ch - '0';
  while (isdigit (ch = next ()))
    lit = 10*lit + (ch - '0');
  if (lit > nvars) perr ("variable %d exceeds maximum %d", lit, nvars);
  assert (nclauses <= sclauses);
  if (nclauses == sclauses) perr ("too many clauses");
  if (lit) lit *= sign, nlits++; else nclauses++;
  if (ch != EOF && !ws (ch)) perr ("expected white space after %d", lit);
  lgladd (lgl, lit);
  last = lit;
  goto BODY;
}

static int isnum (const char * str) {
  const char * p = str;
  if (!isdigit ((int)*p)) return 0;
  while (*++p) if (!isdigit ((int)*p)) return 0;
  return 1;
}

static int exists (const char * str) {
  struct stat buf;
  return !stat (str, &buf);
}

static int term (void * dummy) {
  int res;
  assert (!dummy);
  lockdone ();
  res = done || stop;
  unlockdone ();
  return res;
}

static void produceunit (void * voidptr, int lit) {
  assert (!noparallel);
  lockparunits ();
  assert (parallel.nunits < nvars);
  parallel.units[parallel.nunits++] = lit;
  parallel.produced.units++;
  unlockparunits ();
}

static void consumeunits (void * voidptr, int ** fromptr, int ** toptr) {
  int * consumedptr = voidptr, produced, consumed;
  assert (fromptr);
  assert (toptr);
  lockparunits ();
  produced = parallel.nunits;
  consumed = produced - *consumedptr;
  parallel.consumed.units += consumed;
  unlockparunits ();
  assert (consumed >= 0);
  *fromptr = parallel.units + *consumedptr;
  *toptr = parallel.units + produced;
  *consumedptr = produced;
}

static int intslen (const int * ints) {
  const int * p;
  for (p = ints; *p; p++)
    ;
  return p - ints;
}

static int * appendint (int * ints, int i) {
  int len, * res, * q, other;
  const int * p;
  assert (ints);
  len = intslen (ints) + 1;
  NEW (res, len + 1);
  q = res;
  for (p = ints; (other = *p); p++) *q++ = other;
  assert (p - ints == len - 1);
  *q++ = i;
  assert (q - res == len);
  *q++ = 0;
  return res;
}

static void addint (int ** intsptr, int i) {
  int * oldints = *intsptr, len = intslen (oldints);
  const int * p; int * q, o, * newints;
  NEW (newints, len + 2);
  q = newints;
  for (p = oldints; (o = *p); p++) *q++ = o;
  *q++ = i;
  DEL (oldints, len + 1);
  *intsptr = newints;
}

static void cubemsg (Node * node, const char * str) {
  const int * p;
  if (!verbose) return;
  lockmsg ();
  printf ("c [%d %lld] ", node->depth, (LL) node->id);
  smsg ();
  fputs (str, stdout);
  if (node->cube) {
    for (p = node->cube; *p; p++) printf (" %d", *p);
    fputs (" 0", stdout);
  } else fputs (" <cube-already-deleted>", stdout);
  fputc ('\n', stdout);
  fflush (stdout);
  unlockmsg ();
}

static void initroot () {
  assert (!root);
  msg ("initializing root solver instance");
  root = lglminit (0, alloc, resize, dealloc);
  lglsetopt (root, "druplig", 0);
  lglsetopt (root, "classify", 0);
  if (verbose) lglsetopt (root, "verbose", verbose);
  else if (!showstats) lglsetopt (root, "profile", 0);
  lglsetopt (root, "abstime", 1);
  lglsetopt (root, "trep", 0);
  lglsetopt (root, "compact", 1);
  if (!noparallel) {
    lglsetopt (root, "bca", 0);
    lglseterm (root, term, 0);
    lglsetconsumeunits (root, consumeunits, &rootconsumed);
    lglsetmsglock (root, lockmsg, unlockmsg, 0);
  }
  lglsetime (root, getime);
  lglsetprefix (root, "c (root) ");
}

static Node * newnode (Node * parent, int decision) {
  char prefix[80];
  Node * res;
  locknodes ();
  NEW (res, 1);
  assert (res->state == FREE);
  res->state = READY;
  if (parent) res->depth = parent->depth + 1;
  res->id = ids++;
  res->pos = numnodes;
  PUSH (nodes, res);
  if (numnodes > maxnumnodes) maxnumnodes = numnodes;
  unlocknodes ();
  nmsg (res, "new node");
  if (parent) {
    res->lgl = lglclone (parent->lgl);
    res->decisions = lglgetdecs (res->lgl);
    res->conflicts = lglgetconfs (res->lgl);
    res->propagations = lglgetprops (res->lgl);
    res->consumed = parent->consumed;
    res->cube = appendint (parent->cube, decision);
    lgladd (res->lgl, decision);
    lgladd (res->lgl, 0);
  } else {
    msg ("forking root solver instance");
    NEW (res->cube, 1);
    assert (!intslen (res->cube));
    assert (root);
    res->lgl = lglfork (root);
  }
  sprintf (prefix, "c (%d %lld) ", res->depth, (LL) res->id);
  lglsetprefix (res->lgl, prefix);
  lglseterm (res->lgl, term, 0);
  lglsetmsglock (res->lgl, lockmsg, unlockmsg, 0);
  if (!noparallel) lglsetconsumeunits (res->lgl, consumeunits, &res->consumed);
  cubemsg (res, "opened cube");
  added++;
  return res;
}

static void updstats (Node * node) {
  double * ptr, now, delta;
  LGL * lgl = node->lgl;
  lockstats ();
  decisions += lglgetdecs (lgl) - node->decisions;
  conflicts += lglgetconfs (lgl) - node->conflicts;
  propagations += lglgetprops (lgl) - node->propagations;
  if (started && (ptr = startimeptr)) {
    now = currentime ();
    delta = now - startime;
    startime = now;
    *ptr += delta;
  }
  unlockstats ();
}

static void delnode (Node * node) {
  Node * last;
  int lastpos;
  LGL * lgl;
  assert (node);
  cubemsg (node, "closed cube");
  nmsg (node, "delete node");
  locknodes ();		// TODO why?
  assert (0 <= node->pos && node->pos < sizenodes);
  assert (nodes[node->pos] == node);
  assert (node->state == READY);
  assert (numnodes > 0);
  lastpos = --numnodes;
  last = nodes[lastpos];
  assert (last && last->pos == lastpos);
  if (node != last) { 
    assert (node->pos < lastpos);
    nodes[node->pos] = last;
    last->pos = node->pos;
  }
  nodes[lastpos] = 0;
  unlocknodes ();	// TODO why?
  lgl = node->lgl;
  updstats (node);
  node->lgl = 0;
  if (node->cube) DEL (node->cube, intslen (node->cube) + 1);
  DEL (node, 1);
  assert (lgl);
  if (showstats) lglstats (lgl);
  lglrelease (lgl);
  deleted++;
}

/*------------------------------------------------------------------------*/

static void leafmsg (Leaf * leaf, const char * str) {
  const int * p;
  if (!verbose) return;
  lockmsg ();
  printf ("c %s leaf %lld clause", str, (LL) leaf->id);
  if (leaf->lits) {
    for (p = leaf->lits; *p; p++)
      printf (" %d", *p);
    printf (" 0");
  } else fputs (" <literals-already-deleted>", stdout);
  fputc ('\n', stdout);
  fflush (stdout);
  unlockmsg ();
}

static Leaf * newleaf (Node * node) {
  int * p, lit;
  Leaf * res;
  NEW (res, 1);
  if (++leafs.num > leafs.max) leafs.max = leafs.num;
  res->id = ++leafs.count;
  res->lits = node->cube;
  node->cube = 0;
  for (p = res->lits; (lit = *p); p++) *p = -lit;
  leafmsg (res, "new");
  return res;
}

static void deleaf (Leaf * leaf) {
  leafmsg (leaf, "delete");
  if (leaf->lits) DEL (leaf->lits, intslen (leaf->lits) + 1);
  DEL (leaf, 1);
  assert (leafs.num > 0);
  leafs.count--;
}

static void enqleaf (Leaf * leaf) {
  leafmsg (leaf, "enqueue");
  lockleafs ();
  if (leafs.last) leafs.last->next = leaf;
  else leafs.first = leaf;
  leaf->prev = leafs.last;
  leafs.last = leaf;
  assert (!leaf->next);
  unlockleafs ();
  lockparleafs ();
  parallel.produced.leafs++;
  unlockparleafs ();
}

static Leaf * deqleaf () {
  Leaf * res;
  lockleafs ();
  res = leafs.first;
  if (res) {
    if (res->next) res->next->prev = 0;
    else leafs.last = 0;
    leafs.first = res->next;
  }
  unlockleafs ();
  if (res) leafmsg (res, "dequeue");
  return res;
}

static void enqnewleafromnode (Node * node) { enqleaf (newleaf (node)); }

/*------------------------------------------------------------------------*/

static int * clausetoconsume;

static void consumecls (void * voidptr, int ** cptr, int * glueptr) {
  Leaf * leaf = deqleaf ();
  if (leaf) {
    if (clausetoconsume)
      DEL (clausetoconsume, intslen (clausetoconsume) + 1);
    clausetoconsume = leaf->lits;
    leaf->lits = 0;
    *cptr = clausetoconsume;
    *glueptr = 0;
    deleaf (leaf);
    parallel.consumed.leafs++;
  } else *cptr = 0;
}

static void * runparallel (void * dummy) {
  assert (!dummy);
  int res;
  if ((res = lglsat (parallel.lgl))) {
    vrb ("parallel solver search result %d", res);
    lockdone ();
    parallel.res = res;
    assert (!done || done == res);
    done = parallel.res;
    unlockdone ();
  }
  return dummy;
}

static void startparallel (LGL * lgl) {
  char prefix[80];
  assert (!noparallel);
  assert (!parallel.lgl);
  assert (!parallel.res);
  assert (!parallel.nunits);
  msg ( "cloning and starting %s additional parallel solver instance",
    nparallel ? "second" : "first");
  nparallel++;
  assert (nparallel <= 2);
  NEW (parallel.units, nvars);
  parallel.lgl = lglclone (lgl);
  sprintf (prefix, "c (parallel%d) ", nparallel);
  lglsetprefix (parallel.lgl, prefix);
  lglsetopt (parallel.lgl, "locs", -1);
  lglsetopt (parallel.lgl, "locsbanner", nparallel == 1);
  lglsetopt (parallel.lgl, "locsmaxeff", 1000000);
  lglsetopt (parallel.lgl, "locsmineff", 10000);
  lglsetopt (parallel.lgl, "locsreleff", 20);
  lglsetopt (parallel.lgl, "locsvared", 1000);
  lglsetopt (parallel.lgl, "locswait", 0);
  lglsetopt (parallel.lgl, "block", 1);
  lglsetopt (parallel.lgl, "bca", 0);
  lglseterm (parallel.lgl, term, 0);
  lglsetproduceunit (parallel.lgl, produceunit, 0);
  lglsetconsumecls (parallel.lgl, consumecls, 0);
  lglsetmsglock (parallel.lgl, lockmsg, unlockmsg, 0);
  parallel.decisions = lglgetdecs (parallel.lgl);
  parallel.conflicts = lglgetconfs (parallel.lgl);
  parallel.propagations = lglgetprops (parallel.lgl);
  lockdone ();
  stop = 0;
  unlockdone ();
  if (pthread_create (&parallel.thread, 0, runparallel, 0))
    err ("failed to create thread for additional parallel solver instance");
}

static int joinparallel () {
  int res;
  assert (!noparallel);
  lockdone ();
  stop = 1;
  unlockdone ();
  if (pthread_join (parallel.thread, 0))
    err ("failed to join additional parallel solver instance thread");
  res = parallel.res;
  vrb ("joined parallel solver instance with result %d", res);
  return res;
}

static void releaseparallel () {
  LGL * lgl;
  lockparstats ();
  lgl = parallel.lgl;
  parallel.lgl = 0;
  parallel.res = 0;
  decisions += lglgetdecs (lgl) - parallel.decisions;
  conflicts += lglgetconfs (lgl) - parallel.conflicts;
  propagations += lglgetprops (lgl) - parallel.propagations;
  unlockparstats ();
  if (showstats) lglstats (lgl);
  lglrelease (lgl);
  DEL (parallel.units, nvars);
  parallel.nunits = 0;
  vrb ("released parallel solver instance");
}

/*------------------------------------------------------------------------*/

static char wbuf[80];
static int wlen;

static void wflush () {
  int i;
  fputc ('v', stdout);
  for (i = 0; i < wlen; i++) fputc (wbuf[i], stdout);
  fputc ('\n', stdout);
  wlen = 0;
}

static void wprint (int lit) {
  char str[20];
  int len;
  sprintf (str, " %d", lit);
  len = strlen (str);
  if (wlen + len > 74) wflush ();
  strcpy (wbuf + wlen, str);
  wlen += len;
}

static void witness (LGL * lgl) {
  int idx, lit;
  for (idx = 1; idx <= nvars; idx++) {
    lit = (lglderef (lgl, idx) < 0) ? -idx : idx;
    wprint (lit);
  }
  wprint (0);
  if (wlen) wflush ();
}

/*------------------------------------------------------------------------*/

static int nowfull () { return fullint && !(round % fullint); }

/*------------------------------------------------------------------------*/

static void schedulejob (Node * node, void*(*fun)(void*),
                         const char * name, State state) {
  Job * job;
  js.cnt++;
  assert (nowfull () || numjobs < maxactive);
  assert (state != FREE);
  assert (state != READY);
  NEW (job, 1);
  job->node = node;
  job->fun = fun;
  job->name = name;
  job->state = state;
  job->pos = numjobs;
  PUSH (jobs, job);
  jmsg (job, "scheduled");
}

static void runjob (Job * job) {
  Node * node = job->node;
  jmsg (job, "start");
  assert (node->state == READY);
  node->state = job->state;
  threads++;
  if (pthread_create (&job->thread, 0, job->fun, node))
    err ("failed to create thread job %d %s [%d %lld]",
         job->pos, job->name, node->depth, node->id);
  jmsg (job, "end");
}

static void incworkers () {
  lockworkers ();
  assert (numworkers <= maxworkers);
  while (numworkers >= maxworkers) {
    lock.workers.waited++;
    if (pthread_cond_wait (&workerscond, &lock.workers.mutex))
      err ("failed to wait on decrease of the number of workers");
    lock.workers.waited--;
    assert (numworkers <= maxworkers);
  }
  numworkers++;
  if (numworkers > maxnumworkers) maxnumworkers = numworkers;
  vrb ("number of workers increased to %d", numworkers);
  unlockworkers ();
}

static void decworkers () {
  lockworkers ();
  assert (numworkers > 0);
  numworkers--;
  vrb ("number of workers decreased to %d", numworkers);
  if (pthread_cond_signal (&workerscond))
    err ("failed to signal decrease of the number of workers");
  unlockworkers ();
}

static size_t nodebytes (Node * n) {
  assert (n->state == READY);
  assert (n->lgl);
  return lglbytes (n->lgl);
}

static size_t nodevars (Node * n) {
  assert (n->state == READY);
  assert (n->lgl);
  return lglnvars (n->lgl);
}

static int cmpnodes (Node * m, Node * n, int dir) {
  int res, skipm = skipnode (m), skipn = skipnode (n);
  if ((res = skipm - skipn)) return res;
  if (!skipn && !skipm &&
      (res = nodevars (m) - nodevars (n))) return dir*res;
  if ((res = n->depth - m->depth)) return res;
  if (m->id > n->id) return -1;
  if (m->id < n->id) return 1;
  return 0;
}

static int cmpjobs4qsort (const void * p, const void * q) {
  Job * j = *(Job**)p, * k = *(Job**) q;
  return cmpnodes (j->node, k->node, -1);
}

static void fixjobspos () {
  Job * job;
  int i;
  for (i = 0; i < numjobs; i++) {
    job = jobs[i];
    assert (job);
    job->pos = i;
  }
}

int bytes2mb (size_t bytes) {
  size_t res = bytes;
  res >>= 20;
  if (res > INT_MAX) res = INT_MAX;
  return res;
}

static void printnode (const char * prefix, Node * node) {
  msg ("  %s:  node[%d] = [%d %lld]  (%d vars, %d MB)", 
       prefix, node->pos, node->depth, (LL) node->id,
       nodevars (node), bytes2mb (nodebytes (node)));
}

static void printjobs () {
  char str[80];
  Job * job;
  int i;
  msg ("");
  for (i = 0; i < numjobs; i++) {
    job = jobs[i];
    assert (job);
    assert (job->node);
    assert (job->pos == i);
    sprintf (str, "job[%d] round %d", i, round);
    printnode (str, job->node);
  }
  msg ("");
}

static void sortjobs () {
  qsort (jobs, numjobs, sizeof *jobs, cmpjobs4qsort);
  fixjobspos ();
  if (verbose) printjobs ();
}

static void runjobs () {
  Job * job;
  int i;
  sortjobs ();
  vrb ("running %d jobs", numjobs);
  numworkers = 0;
  for (i = 0; i < numjobs; i++) {
    incworkers ();
    job = jobs[i];
    runjob (job);
  }
  vrb ("started running %d jobs", numjobs);
}

/*------------------------------------------------------------------------*/

static void joinjob (Job * job) {
  Node * node = job->node;
  assert (node->state != FREE);
  assert (node->state != READY);
  if (node->state == SEARCH) mmsg ("join search", node);
  else if (node->state == SIMP) mmsg ("join simp", node);
  else if (node->state == LKHD) mmsg ("join simp", node);
  else assert (node->state == SPLIT), mmsg ("join split", node);
  if (pthread_join (job->thread, 0))
    err ("failed to join thread");
  node->state = READY;
  if (node->res == 20) {
    mmsg ("unsatisfiable", node);
  } else if (node->res == 10) {
    lockdone ();
    mmsg ("satisfiable", node);
    done = 10;
    unlockdone ();
  } else {
    assert (!node->res);
    mmsg ("unknown", node);
  }
  DEL (job, 1);
}

static void joinjobs () {
  int i;
  vrb ("joining %d jobs in round %d", numjobs, round);
  for (i = 0; i < numjobs; i++) joinjob (jobs[i]);
  vrb ("finished joining %d nodes in round %d", numjobs, round);
  numjobs = 0;
}

/*------------------------------------------------------------------------*/

static int incmpnodes (const void * p, const void * q) {
  return cmpnodes (*(Node**)p, *(Node**)q, 1);
}

static int decmpnodes (const void * p, const void * q) {
  return cmpnodes (*(Node**)p, *(Node**)q, -1);
}

static void printnodes (const char * prefix) {
  Node * node;
  int i;
  msg ("");
  for (i = 0; i < numnodes; i++) {
    node = nodes[i];
    if (skipnode (node)) continue;
    printnode (prefix, node);
  }
  msg ("");
}

static void fixnodespos () {
  Node * node;
  int i;
  for (i = 0; i < numnodes; i++) {
    node = nodes[i];
    assert (node);
    node->pos = i;
  }
}

static void sortnodes (const char * name,
                       int (*cmp)(const void *, const void*)) {
  char prefix[80];
  qsort (nodes, numnodes, sizeof *nodes, cmp);
  fixnodespos ();
  if (!verbose) return;
  sprintf (prefix, "%s(%d) sorted", name, round);
  printnodes (prefix);
}

/*------------------------------------------------------------------------*/

static void * simpnode (void * voidptr) {
  Node * node = voidptr;
  assert (!node->res);
  assert (node->state == SIMP);
  nmsg (node, "simp");
  node->res = lglsimp (node->lgl, 1);
  node->simplified = 1;
  nmsg (node, "simp result %d", node->res);
  if (node->res == 10) lockdone (), done = 10, unlockdone ();
  else if (node->res == 20) {
    locksimplified ();
    simplified++;
    sumsimplified++;
    unlocksimplified ();
  }
  decworkers ();
  return node;
}

static void simp () {
  Node * node;
  int i, l;
  if (nosimp) return;
  LOG ("simp");
  startimer (&wct.simp);
  startphase ("simp");
  sortnodes ("simp", incmpnodes);
  l = maxactive;
  if (nowfull ()) l *= FULLSIMP;
  if (l > numnodes) l = numnodes;
  msg (" %d simplify  %d out of %d nodes %.0f%%",
    round, l, numnodes, pcnt (l, numnodes));
  for (i = 0; i < numnodes && numjobs < l; i++) {
    node = nodes[i];
    assert (node);
    if (skipnode (node)) continue;
    if (!forcesimp && node->simplified) continue;
    js.simp++;
    schedulejob (node, simpnode, "simp", SIMP);
  }
  vrb ("scheduled %d simplification jobs in round %d", numjobs, round);
  runjobs ();
  joinjobs ();
  stoptimer ();
}

/*------------------------------------------------------------------------*/

void mysrand (unsigned long long seed) {
  unsigned z = seed >> 32, w = seed;
  if (!z) z = ~z; if (!w) w = ~w;
  rng.z = z, rng.w = w;
}

static unsigned myrand () {
  unsigned res;
  rng.z = 36969 * (rng.z & 65535) + (rng.z >> 16);
  rng.w = 18000 * (rng.w & 65535) + (rng.w >> 16);
  res = (rng.z << 16) + rng.w;
  return res;
}

static unsigned myrandmod (unsigned mod) {
  unsigned res;
  assert (mod >= 1);
  if (mod <= 1) return 0;
  res = myrand ();
  res %= mod;
  return res;
}

/*------------------------------------------------------------------------*/

static void * lookaheadnode (void * voidptr) {
  int oldvars, newvars, redpermille, oldjwhred;
  Node * node = voidptr;
  assert (!node->res);
  assert (node->state == LKHD);
  oldjwhred = lglgetopt (node->lgl, "jwhred");
  oldvars = lglnvars (node->lgl);
  if (!stoplkhd) {
    nmsg (node, "full tree-based lookahead");
    lglsetopt (node->lgl, "lkhd", 2);
  } else if (locslkhd) {
    nmsg (node, "local search based lookahead");
    lglsetopt (node->lgl, "lkhd", -1);
  } else {
    nmsg (node, "no lookahead");
    lglsetopt (node->lgl, "lkhd", 1);	// JWH
    lglsetopt (node->lgl, "jwhred", 2);
  }
  node->lookahead = lglookahead (node->lgl);
  lglsetopt (node->lgl, "jwhred", oldjwhred);
  nmsg (node, "lookahead literal %d", node->lookahead);
  newvars = lglnvars (node->lgl);
  assert (newvars <= oldvars);
  if (!oldvars) redpermille = 0;
  else 
    redpermille = 
      (1000ll * (long long) (oldvars -  newvars)) / (long long) oldvars;
  nmsg (node, "lookahead reduced %d variables to %d variables %.1f%%", 
    oldvars, newvars, redpermille / 10.0);
  if (redpermille >= STOPLKHDRED) lkhdsuccessful = 1;
  decworkers ();
  return node;
}

static int firstlkhd () {
  int numactive = (maxactive > numnodes) ? numnodes : maxactive;
  numtosplit = (numactive * branches + 99) / 100;
  if (numnodes < maxactive) {
    int delta = maxactive - numnodes;
    if (numtosplit <= delta) numtosplit = delta;
    if (numtosplit > numnodes) numtosplit = numnodes;
  }
  if (asymmetric) {
    lastosplit = numactive - 1;
    firstosplit = lastosplit - numtosplit + 1;
  } else {
    firstosplit = 0;
    lastosplit = numtosplit - 1;
    if (numtosplit > numnodes) lastosplit = numnodes-1;
  }
  msg (
    " %d lookahead %d out of %d nodes %.0f%% on nodes [%d..%d]",
    round,
    numtosplit, numnodes,
    pcnt (numtosplit, numnodes),
    firstosplit, lastosplit);
  fflush (stdout);
  return firstosplit;
}

static int donelkhd (int i) {
  assert (firstosplit <= i), assert (i <= lastosplit + 1);
  return i > lastosplit;
}

static int nextlkhd (int i) { return i + 1; }

static void lookahead () {
  size_t sumbytes = currentbytes, expected;
  int i, j, k;
  Node * node;
  LOG ("lookahead");
  startimer (&wct.lkhd);
  lkhdsuccessful = 0;
  for (i = 0; i < numnodes; i++) {
    node = nodes[i];
    assert (node);
    node->lookahead = 0;
  }
  startphase ("lookahead");
  sortnodes ("lookahead", balance ? decmpnodes : incmpnodes);
  if (randswap) {
    vrb ("swapping %d random node pairs", randswap);
    for (k = 0; k < randswap; k++) {
      i = myrandmod (numnodes);
      j = myrandmod (numnodes);
      vrb ("swapping nodes %d and %d", i, j);
      node = nodes[i];
      nodes[i] = nodes[j];
      nodes[j] = node;
    }
    fixnodespos ();
  }
  vrb ("starting lookahead round %d", round);
  for (i = firstlkhd (); !donelkhd (i); i = nextlkhd (i)) {
    node = nodes[i];
    expected = nodebytes (node);
    nmsg (node, "cloning might add %d MB", bytes2mb (expected));
    nmsg (node, "plus already scheduled %d MB gives %d MB",
	  bytes2mb (sumbytes), bytes2mb (expected + sumbytes));
    if (sumbytes + expected > softlimbytes) {
      vrb ("resulting size %d MB exceeds soft memory limit %d MB",
           bytes2mb (sumbytes + expected), bytes2mb (softlimbytes));
      if (!splitlimbytes || splitlimbytes > sumbytes)
	splitlimbytes = sumbytes;
    } else {
      sumbytes += expected;
      if (!stoplkhd) js.lkhd++;
      schedulejob (node, lookaheadnode, "lookahead", LKHD);
    }
  }
  vrb ("final size is expected not to exceed %d MB", bytes2mb (sumbytes));
  vrb ("scheduled %d lookahead jobs out of %d in round %d", 
       numjobs, numnodes, round);
  runjobs ();
  joinjobs ();
  if (stoplkhd) stoplkhd--;
  else if (!forcelkhd && !lkhdsuccessful) {
    assert (0 < stoplkhdint && stoplkhdint <= MAXSTOPLKH);
    if (stoplkhdint < MAXSTOPLKH) stoplkhdint *= 2;
    stoplkhd = stoplkhdint;
  } else if (stoplkhdint > 1) stoplkhdint--;
  stoptimer ();
}

/*------------------------------------------------------------------------*/

static void * splitnode (void * voidptr) {
  Node * node = voidptr;
  Node * child;
  assert (node->state == SPLIT);
  child = newnode (node, -node->lookahead);
  addint (&node->cube, node->lookahead);
  cubemsg (node, "extended");
  lgladd (node->lgl, node->lookahead);
  lgladd (node->lgl, 0);
  nmsg (node, "light simplification");
  (void) lglsimp (node->lgl, 0);
  if (reducecache) {
    nmsg (child, "reducing cache");
    lglreducecache (child->lgl);
    lglreducecache (node->lgl);
  }
  nmsg (child, "light simplification");
  (void) lglsimp (child->lgl, 0);
  nmsg (node, "cloned and lightly simplified node and child");
  child->simplified = node->simplified = 0;
  decworkers ();
  splitsuccessful = 1;
  return node;
}

static void split () {
  int i, tosplit = 0, found = 0;
  Node * node;
  LOG ("split");
  startimer (&wct.split);
  startphase ("split");
  splitsuccessful = 0;
  for (i = 0; i < numnodes; i++) {
    node = nodes[i];
    assert (node);
    tosplit += (node->lookahead != 0);
  }
  msg (" %d splitting %d out of %d nodes %.0f%%", 
    round, tosplit, numnodes, pcnt (tosplit, numnodes));
  assert (!numjobs);
  for (i = 0; i < numnodes; i++) {
    node = nodes[i];
    if (skipnode (node)) continue;
    found++;
    if (!node->lookahead) continue;
    js.split++;
    schedulejob (node, splitnode, "split", SPLIT);
  }
  vrb ("scheduled %d cloning jobs for %d / %d nodes in round %d",
       numjobs, tosplit, found, round);
  runjobs ();
  joinjobs ();
  stoptimer ();
}

/*------------------------------------------------------------------------*/

static void varspan (int * minvarsptr, int * actvarsptr, int * maxvarsptr) {
  int i, minvars, actvars, maxvars, j;
  sortnodes ("varspan", incmpnodes);
  for (i = 0; i < numnodes; i++)
    if (!skipnode (nodes[i])) break;
  minvars = (i < numnodes) ? lglnvars (nodes[i]->lgl) : 0;
  for (i = numnodes-1; i >= 0; i--)
    if (!skipnode (nodes[i])) break;
  maxvars = (i < 0) ? 0 : lglnvars (nodes[i]->lgl);
  j = 0;
  for (i = 0; i < numnodes; i++)
    if (!skipnode (nodes[i]) && ++j == maxactive)
      break;
  actvars = (j == maxactive) ? lglnvars (nodes[i]->lgl) : maxvars;
  *minvarsptr = minvars;
  *actvarsptr = actvars;
  *maxvarsptr = maxvars;
}

static void report () {
  int minvars, maxvars, actvars, isfull = nowfull ();
  double t = getime (), m;
  lockmem ();
  m  = currentbytes/(double)(1<<20);
  unlockmem ();
  varspan (&minvars, &actvars, &maxvars);
  msg ("%c%d %lld%c %.1f sec, "
    "%.0f MB, %d nodes +%d -%d s%d, vars[%d..%d..%d]",
    (isfull ? '[' : '('), round, (LL) ids, (isfull ? ']' : ')'), t, m,
    numnodes, added, deleted, simplified, minvars, actvars, maxvars);
}

/*------------------------------------------------------------------------*/

typedef struct Opt { const char * name; int val; } Opt;

static Opt lopts[] = {
  { "restartint", 4 },
  { "restartint", 1000 },
  { "phase", -1 },
};

static const int nopts = sizeof lopts / sizeof *lopts;

static unsigned long long hashtwo64 (unsigned long long a,
                                     unsigned long long b) {
  return 123369937ull * a + 4443739543 * b + 346961;
}

static void setopts (Node * node) {
  unsigned long long r = hashtwo64 (node->id, round);
  int def = !(r%2), newval, first;
  LGL * lgl = node->lgl;
  Opt * o;
  lockopts ();
  first = !opts.set++;
  if (def) opts.def++;
  unlockopts ();
  if (first) {
    msg ("");
    msg ("using %d options for portfolio solving:", nopts);
    msg ("");
    for (o = lopts; o < lopts + nopts; o++)
      msg ("  --%s=%d", o->name, o->val);
    msg ("");
  }
  if (def) {
    for (o = lopts; o < lopts + nopts; o++) {
      newval = lgldefopt (lgl, o->name);
      if (lglgetopt (lgl, o->name) == newval) continue;
      nmsg (node,
	"resetting option --%s=%d (default)",
	o->name, newval);
      lglsetopt (lgl, o->name, newval);
    }
  } else {
    o = lopts + (r % nopts);
    nmsg (node,
      "setting option --%s=%d%s",
      o->name, o->val, (def ? " (default)" : ""));
    lglsetopt (lgl, o->name, o->val);
    lglsetopt (lgl, "seed", def);
  }
}

static void * searchnode (void * voidptr) {
  Node * node = voidptr;
  int64_t oldconfs = lglgetconfs (node->lgl), deltaconfs;
  assert (!node->res);
  assert (node->state == SEARCH);
  if (portfolio) setopts (node);
  nmsg (node, "search for %d conflicts", thisclim);
  lglsetopt (node->lgl, "clim", thisclim);
  node->res = lglsat (node->lgl);
  node->simplified = 0;
  nmsg (node, "search result %d", node->res);
  if (node->res == 10) lockdone (), done = 10, unlockdone ();
  decworkers ();
  lockconfs ();
  deltaconfs = lglgetconfs (node->lgl) - oldconfs;
  PUSH (confstack, deltaconfs);
  unlockconfs ();
  return node;
}

static int cmpint64 (const void * p, const void * q) {
  int64_t a = *(int64_t*) p, b = *(int64_t*) q;
  if (a < b) return -1;
  if (a > b) return 1;
  return 0;
}

static void search () {
  int64_t avgconfs, medianconfs;
  Node * node;
  int i, l;
  LOG ("search");
  lockconfs ();			// TODO why?
  numconfstack = 0;
  if (eager &&
      clim > minclim &&
      numnodes < maxactive &&
      (!softlimbytes || currentbytes < softlimbytes)) {
    assert (minclim > 0);
    forcedclim = minclim;
    vrb (" %d forcing limit %d instead of %d", round, forcedclim, clim);
  } else forcedclim = 0;
  thisclim = forcedclim ? forcedclim : clim;
  newclim = clim;
  unlockconfs ();
  if (nosearch) return;
  startimer (&wct.search);
  startphase ("search");
  sortnodes ("search", incmpnodes);
  l = maxactive;
  if (nowfull ()) l *= FULLSEARCH;
  if (l > numnodes) l = numnodes;
  msg (" %d searching %d out of %d nodes %.0f%% limit %d%s",
    round, l, numnodes, pcnt (l, numnodes), thisclim,
    forcedclim ? " forced" : "");
  for (i = 0; i < numnodes && numjobs < l; i++) {
    node = nodes[i];
    assert (node);
    if (skipnode (node)) continue;
    js.search++;
    schedulejob (node, searchnode, "search", SEARCH);
  }
  vrb ("scheduled %d search jobs in round %d", numjobs, round);
  runjobs ();
  joinjobs ();
  lockconfs ();			// TODO why?
  assert (numconfstack >= 0);
  if (numconfstack) {
    avgconfs = 0;
    for (i = 0; i < numconfstack; i++) avgconfs += confstack[i];
    avgconfs = numconfstack ? avgconfs / numconfstack : 0;
    qsort (confstack, numconfstack, sizeof *confstack, cmpint64);
    medianconfs = confstack[numconfstack/2];
    msg (" %d average actual conflicts %lld median %lld",
      round, (LL) avgconfs, (LL) medianconfs);
    newclim = medianconfs;
  }
  unlockconfs ();
  stoptimer ();
}

/*------------------------------------------------------------------------*/

static void updateclim () {
  int oldclim = clim, delta;
  assert (oldclim);
  if (forcedclim) { forcedclim = 0; forcedclims++; goto DONE; }
  assert (simplified <= deleted);
  delta = deleted - simplified;
  if (delta > added || !splitsuccessful) newclim *= 2;
  else if (!delta) newclim /= 2;
  else if (delta < added) newclim = (9*newclim)/10;
  if (newclim < minclim) clim = minclim;
  else if (newclim > maxclim) clim = maxclim;
  else clim = newclim;
  if (newclim < oldclim) declims++;
  if (newclim > oldclim) inclims++;
DONE:
  if (oldclim != clim) vrb ("new conflict limit set to %d", clim);
  else vrb ("conflict limit remains at %d", clim);
  sumclims += clim;
  added = deleted = simplified = 0;
}

/*------------------------------------------------------------------------*/

static int flush () {
  int i, before = numnodes, flushed, res;
  Node * node;
  i = 0; 
  LOG ("flush");
  while (i < numnodes) {
    node = nodes[i];
    assert (node);
    assert (node->state == READY);
    if (node->res == 20) enqnewleafromnode (node), delnode (node);
    else if (lglinconsistent (node->lgl)) {
      mmsg ("inconsistent", node);
      enqnewleafromnode (node), delnode (node);
    } else i++;
  }
  if ((flushed = before - numnodes)) vrb ("flushed %d nodes", flushed);
  if (numnodes) {
    res = 0;
    vrb ("still %d nodes left", numnodes);
  } else {
    res = 20;
    msg ("no more nodes left");
    printf ("s UNSATISFIABLE\n");
    fflush (stdout);
  }
  return res;
}

/*------------------------------------------------------------------------*/

static void mergestats () {
  Node * node;
  int i;
  for (i = 0; i < numnodes; i++) {
    node = nodes[i];
    assert (node);
    assert (node->state != FREE);
    if (node->lgl) updstats (node);
  }
  lockparstats ();
  if (!noparallel && parallel.lgl) {
    decisions += lglgetdecs (parallel.lgl) - parallel.decisions;
    conflicts += lglgetconfs (parallel.lgl) - parallel.conflicts;
    propagations += lglgetprops (parallel.lgl) - parallel.propagations;
  }
  unlockparstats ();
}

static void stats () {
  double w = getime (), t = lglprocesstime ();
  msg ("");
  msg ("%d rounds, %lld nodes (%d max), %lld threads (%d max)", 
       round, (LL) ids, maxnumnodes,
       !noparallel + (LL) threads, maxnumworkers + !noparallel);
  msg ("%.0f avg clim, %lld incs, %lld decs, %lld forced, %lld simp", 
      avg (sumclims, inclims + declims + 1),
      (LL) inclims, (LL) declims,
      (LL) forcedclims, (LL) sumsimplified);
  msg ("");
  msg ("%.2f wall clock time, %.2f process time", w, t);
  msg ("%.0f%% utilization for %d%s worker threads on %d cores",
       pcnt (t, (maxworkers + !noparallel) * w), maxworkers,
       (!noparallel ? " + 1" : ""), ncores);
  msg ("");
  if (!noparallel) {
    msg ("started %d additional parallel solver instances", nparallel);
    msg ("%lld units consumed %.0f%% of %lld produced",
      (LL) parallel.consumed.units,
      pcnt (parallel.consumed.units, parallel.produced.units),
      (LL) parallel.produced.units);
    msg ("%lld leaf clauses consumed %.0f%% of %lld produced",
      (LL) parallel.consumed.leafs,
      pcnt (parallel.consumed.leafs, parallel.produced.leafs),
      (LL) parallel.produced.leafs);
  }
  msg ("");
  msg ("%lld conflicts, %.0f conflicts per second", 
       (LL) conflicts, avg (conflicts, w));
  msg ("%lld decisions, %.0f decisions per second",
       (LL) decisions, avg (decisions, w));
  msg ("%lld propagations, %.1f million propagations per second",
       (LL) propagations, avg (propagations/1e6, w));
  msg ("");
  msg ("%7d %3.0f%% lookaheads      %7.2f seconds %4.0f%%",
    js.lkhd, pcnt (js.lkhd, js.cnt), wct.lkhd, pcnt (wct.lkhd,w));
  msg ("%7d %3.0f%% splits          %7.2f seconds %4.0f%%",
    js.split, pcnt (js.split, js.cnt), wct.split, pcnt (wct.split,w));
  msg ("%7d %3.0f%% simplifications %7.2f seconds %4.0f%%",
    js.simp, pcnt (js.simp, js.cnt), wct.simp, pcnt (wct.simp,w));
  msg ("%7d %3.0f%% searches        %7.2f seconds %4.0f%%",
    js.search, pcnt (js.search, js.cnt), wct.search, pcnt (wct.search,w));
  msg ("======================================================");
  msg ("%7d 100%% scheduled jobs  %7.2f seconds, %.0f MB",
    js.cnt, w, maxbytes / (double)(1<<20));
}

/*------------------------------------------------------------------------*/

static void parallelwins (int res) {
  assert (!noparallel);
  msg ("%s parallel solver instance wins with result %d after "
       "%.2f seconds", nparallel == 1 ? "first" : "second", res, getime ());
  if (res == 10) {
    printf ("s SATISFIABLE\n");
    if (!nowitness) {
      fflush (stdout);
      if (nparallel > 1) {
	lgljoin (root, parallel.lgl);
	witness (root);
      } else witness (parallel.lgl);
    }
  } else if (res == 20) printf ("s UNSATISFIABLE\n");
  else printf ("c s UNKNOWN\n"), abort ();		// TODO unreachable?
  fflush (stdout);
}

static int finish () {
  Node * node;
  int i, res;
  LOG ("finish");
  if ((res = flush ())) return res;
  for (i = 0; i < numnodes; i++) {
    node = nodes[i];
    assert (node);
    assert (node->state == READY);
    assert (node->lgl);
    if (node->res == 10) break;
  }
  if (i < numnodes) {
    vrb ("node %d sat", i);
    res = 10;
    node = nodes[i];
    assert (node);
    assert (node->res == 10);
    msg ("winner [%d %lld] satisfiable in round %d "
         "after %lld nodes and %.2f seconds",
      node->depth, (LL) node->id, round, (LL) ids, getime ());
    printf ("s SATISFIABLE\n");
    if (!nowitness) {
      fflush (stdout);
      assert (node->lgl != root);
      lgljoin (root, node->lgl);
      witness (root);
    }
    fflush (stdout);
  } else if (!noparallel) {
    lockdone ();
    res = parallel.res;
    unlockdone ();
    vrb ("no node result thus parallel result %d", res);
    if (res) parallelwins (res);
  } else {
    assert (numnodes > 0);
#ifndef NDEBUG
    for (i = 0; i < numnodes; i++) {
      node = nodes[i];
      assert (node);
      assert (node->state == READY);
      assert (!node->res);
      assert (node->lgl);
      assert (!lglinconsistent (node->lgl));
    }
#endif
    vrb ("no satisfiable node found but still %d nodes left in round %d", 
	 numnodes, round);
  }
  return res;
}

/*------------------------------------------------------------------------*/

static int catchedsig;

static void (*sig_int_handler)(int);
static void (*sig_segv_handler)(int);
static void (*sig_abrt_handler)(int);
static void (*sig_term_handler)(int);

static void resetsighandlers () {
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
    mergestats ();
    stats ();
    caughtsigmsg (sig);
  }
  resetsighandlers ();
  raise (sig);
}

static void setsighandlers () {
  sig_int_handler = signal (SIGINT, catchsig);
  sig_segv_handler = signal (SIGSEGV, catchsig);
  sig_abrt_handler = signal (SIGABRT, catchsig);
  sig_term_handler = signal (SIGTERM, catchsig);
}

/*------------------------------------------------------------------------*/

static void init () {
  wct.epoch = currentime ();

  pthread_mutex_init (&lock.confs.mutex, 0);
  pthread_mutex_init (&lock.done.mutex, 0);
  pthread_mutex_init (&lock.leafs.mutex, 0);
  pthread_mutex_init (&lock.mem.mutex, 0);
  pthread_mutex_init (&lock.msg.mutex, 0);
  pthread_mutex_init (&lock.nodes.mutex, 0);
  pthread_mutex_init (&lock.parleafs.mutex, 0);
  pthread_mutex_init (&lock.parstats.mutex, 0);
  pthread_mutex_init (&lock.parunits.mutex, 0);
  pthread_mutex_init (&lock.simplified.mutex, 0);
  pthread_mutex_init (&lock.stats.mutex, 0);
  pthread_mutex_init (&lock.workers.mutex, 0);

  pthread_cond_init (&workerscond, 0);
}

static int has (const char * str, const char * suffix) {
  int l = strlen (str), k = strlen (suffix);
  if (l < k) return 0;
  return !strcmp (str + l - k, suffix);
}

static FILE * cmd (const char * fmt, const char * name) {
  FILE * res;
  char * s = malloc (strlen (fmt) + strlen (name) + 1);
  sprintf (s, fmt, name);
  res = popen (s, "r");
  free (s);
  return res;
}

static int parselopt (const char * arg, int * resptr, const char * opt) {
  const char * p, * q;
  if (arg[0] != '-' && arg[1] != '-') return 0;
  p = arg + 2, q = opt;
  while (*p == *q) p++, q++;
  if (*q) return 0;
  if (*p++ != '=') return 0;
  if (*resptr) err ("multiple '--%s=...' options", opt);
  if (!isnum (p))
    err ("expected number as argument in '%s'", arg);
  if ((*resptr = atoi (p)) <= 0) 
    err ("expected positive number in '%s'", arg);
  return 1;
}

#define U64_MAX ((~0ull)>>1)

int parseu64 (const char * arg, const char * opt) {
  unsigned long long res = 0;
  const char * p = arg;
  int ch;
  ch = *p++;
  if (!ch) err ("empty string argument to '%s'", opt);
  res = ch - '0';
  while (isdigit (ch = *p++)) {
    int digit;
    if (U64_MAX/10 < res)
TOOLARGE:
      err ("argument to '%s' too large", opt);
    res *= 10;
    assert (res > 0);
    digit = ch - '0';
    if (U64_MAX - digit < res) goto TOOLARGE;
    res += digit;
    assert (res > 0);
  }
  if (ch) err ("invalid unsigned 64 bit number in '%s %s'", opt, arg);
  assert (res > 0);
  return res;
}

/*------------------------------------------------------------------------*/

int main (int argc, char ** argv) {
  unsigned long long seed = 0;
  int i, res, tmp, clf;
  Node * node;
  Leaf * leaf;
  double sec;
  init ();
  for (i = 1; i < argc; i++) {
    if (!strcmp (argv[i], "-h")) usage ();
    else if (!strcmp (argv[i], "--version")) version ();
    else if (!strcmp (argv[i], "-v")) verbose++;
    else if (!strcmp (argv[i], "-S")) showstats = 1;
    else if (!strcmp (argv[i], "-n")) nowitness = 1;
    else if (!strcmp (argv[i], "-O")) optimize = 1;
    else if (argv[i][0] == '-' && argv[i][1] == 'O' && isnum (argv[i]+2)) {
      if (optimize >= 0) err ("multiple '-O...' options");
      if ((optimize = atoi (argv[i] + 2)) < 0)
	err ("invalid number in '%s'", argv[i]);
    } else if (!strcmp (argv[i], "--reduce")) reducecache = 1;
    else if (!strcmp (argv[i], "--locslkhd")) locslkhd = 1;
    else if (!strcmp (argv[i], "--force-simp")) forcesimp = 1;
    else if (!strcmp (argv[i], "--no-force-lkhd")) forcelkhd = 0;
    else if (!strcmp (argv[i], "--no-simp")) nosimp = 1;
    else if (!strcmp (argv[i], "--no-search")) nosearch = 1;
    else if (!strcmp (argv[i], "--no-parallel")) noparallel = 1;
    else if (!strcmp (argv[i], "--no-full")) fullint = 0;
    else if (!strcmp (argv[i], "-f")) {
      if (++i == argc) err ("argument to '-f' missing");
      if (!isnum (argv[i]) || (fullint = atoi (argv[i])) < 0)
	err ("expected (non negative) number as argument in '-f %s'",
	     argv[i]);
    } else if (!strcmp (argv[i], "--balance")) balance = 1;
    else if (!strcmp (argv[i], "--symmetric")) asymmetric = 0;
    else if (!strcmp (argv[i], "--asymmetric")) asymmetric = 1;
    else if (!strcmp (argv[i], "--portfolio")) portfolio = 1;
    else if (!strcmp (argv[i], "--eager")) eager = 1;
    else if (!strcmp (argv[i], "--lazy")) eager = 0;
    else if (!strcmp (argv[i], "-m")) {
      if (hardlimbytes) err ("multiple memory limit specification");
      if (++i == argc) err ("argument to '-m' missing");
      if (!isnum (argv[i]))
	err ("expected number as argument in '-m %s'", argv[i]);
      hardlimbytes = (size_t) atoll (argv[i]);
      if (hardlimbytes > 0) hardlimbytes <<= 20;
      if (hardlimbytes <= 0) err ("invalid number of MB in '-m %s'", argv[i]);
    } else if (!strcmp (argv[i], "-g")) {
      if (hardlimbytes) err ("multiple memory limit specification");
      if (++i == argc) err ("argument to '-g' missing");
      if (!isnum (argv[i]))
	err ("expected number as argument in '-g %s'", argv[i]);
      hardlimbytes = (size_t) atoll (argv[i]);
      if (hardlimbytes > 0) hardlimbytes <<= 30;
      if (hardlimbytes <= 0) err ("invalid number of GB in '-g %s'", argv[i]);
    } else if (!strcmp (argv[i], "-t")) {
      if (maxworkers > 0) err ("multiple '-t <workers>' options");
      if (maxworkers2 > 0) err ("both '-t <workers>' and '<workers>'");
      if (++i == argc) err ("argument to '-t' missing");
      if (!isnum (argv[i]))
	err ("expected number as argument in '-t %s'", argv[i]);
      if ((maxworkers = atoi (argv[i])) <= 0) 
	err ("expected positive number in '-t %s'", argv[i]);
    } else if (!strcmp (argv[i], "-a")) {
      if (maxactive > 0) err ("multiple '-a <nodes>' options");
      if (++i == argc) err ("argument to '-a' missing");
      if (!isnum (argv[i]))
	err ("expected number as argument in '-a %s'", argv[i]);
      if ((maxactive = atoi (argv[i])) <= 0) 
	err ("expected positive number in '-a %s'", argv[i]);
    } else if (!strcmp (argv[i], "-b")) {
      if (branches >= 0) err ("multiple '-b <branches>' options");
      if (++i == argc) err ("argument to '-b' missing");
      if (!isnum (argv[i]))
	err ("expected number as argument in '-b %s'", argv[i]);
      if ((branches = atoi (argv[i])) <= 0) 
	err ("expected positive number in '-b %s'", argv[i]);
      if (branches > 100)
	err ("expected number not larger than 100 in '-b %s'", argv[i]);
    } else if (!strcmp (argv[i], "-r")) {
      if (randswap > 0) err ("multiple '-r <posnum>' options");
      if (++i == argc) err ("argument to '-r' missing");
      if (!isnum (argv[i]))
	err ("expected number as argument in '-r %s'", argv[i]);
      if ((randswap = atoi (argv[i])) <= 0) 
	err ("expected positive number in '-r %s'", argv[i]);
    } else if (!strcmp (argv[i], "-s")) {
      if (++i == argc) err ("argument to '-s' missing (try '-h')");
      seed = parseu64 (argv[i], "-s");
    } else if (parselopt (argv[i], &minclim, "min")) ;
    else if (parselopt (argv[i], &initclim, "init")) ;
    else if (parselopt (argv[i], &maxclim, "max")) ;
    else if (*argv[i] == '-')
      err ("invalid command line option '%s' (try '-h')", argv[i]);
    else if (!fname && isnum (argv[i]))
      err ("<file> file name can not be a positive number '%s'", argv[i]);
    else if (fname && maxworkers2)
      err ("too many arguments (including <file> and <workers>)");
    else if (fname && !isnum (argv[i]))
      err ("expected positive number for <workers> but got '%s'", argv[i]);
    else if (fname) {
      if ((maxworkers2 = atoi (argv[i])) <= 0)
	err ("invalid number '%s' for <workers>", argv[i]);
    } else if (!exists (argv[i])) err ("can not stat file '%s'", argv[i]);
    else fname = argv[i];
  }
  if (maxworkers2) assert (!maxworkers), maxworkers = maxworkers2;
  if (!fname) file = stdin, fname = "<stdin>", clf = 0;
  else if (has (fname, ".gz")) file = cmd ("gunzip -c %s", fname), clf = 2;
  else if (has (fname, ".xz")) file = cmd ("xz -d -c %s", fname), clf = 2;
  else if (has (fname, ".bz2")) file = cmd ("bzcat %s", fname), clf = 2;
  else if (has (fname, ".7z"))
    file = cmd ("7z x -so %s 2>/dev/null", fname), clf = 2;
  else file = fopen (fname, "r"), clf = 1;
  if (!file) err ("can not read '%s'", fname);
  lglbnr ("Treengeling Cube and Conquer SAT Solver", "c ", stdout);
  msg ("verbose level %d", verbose);
  msg ("will %sprint statistics for each solver instance",
       showstats ? "" : "NOT ");
  msg ("will %sprint satisfying assignment", nowitness ? "NOT " : "");
  msg ("");
  if (balance) msg ("splitting large nodes first ('-b' option)");
  else msg ("splitting small nodes first (no '-b' option)");
  if (asymmetric) msg ("asymmetric splitting ('--asymmetric')");
  else msg ("symmetric splitting ('--symmetric')");
  if (portfolio) msg ("portfolio option fuzzing enabled ('--portfolio'");
  else msg ("portfolio option fuzzing disabled (no '--portfolio' option)");
  if (fullint) msg ("full search/simplification round interval %d", fullint);
  else msg ("no full search/simplification");

  if (randswap) {
    msg ("will swap %d nodes randomly during lookahead", randswap);
    msg ("random seed %llu", (unsigned long long) seed);
    mysrand (seed);
  } else msg ("no randomization in lookahead");

  msg ("");
  ncores = getcores (1);

  if (!maxworkers) {
   maxworkers = CORES2WORKERS (ncores);
   msg ("maximum %d workers (no '-t <worker>' option)", maxworkers);
  } else if (maxworkers2)
    assert (maxworkers2 == maxworkers),
    msg ("maximum %d workers as specified ('%d')", maxworkers, maxworkers2);
  else
    msg ("maximum %d workers as specified ('-t %d')", maxworkers, maxworkers);

  if (noparallel)
    msg ("not using additional parallel solver instances ('--no-parallel')");
  else if (maxworkers == 1) {
    msg ("not using additional parallel solver instances ('-t 1')");
    noparallel = 1;
  } else {
    msg ("using one worker for additional parallel solver instance");
    maxworkers--;
  }

  if (!maxactive) {
    maxactive = WORKERS2ACTIVE (maxworkers);
    msg ("maximum of %d active nodes (no '-a <nodes>' option)", 
         maxactive, maxactive);
  } else 
    msg ("maximum of %d active nodes as specified ('-a %d')",
	 maxactive, maxactive);

  if (branches >= 0) {
    msg ("ratio of split nodes is %d%% as specified ('-b %d')",
         branches, branches);
  } else
    msg ("default ratio of split nodes is %d%%", branches = DEFBRANCHES);

  msg ("");
  if (!hardlimbytes)  {
    hardlimbytes = getotalmem (1);
    msg ("hard memory limit of %d MB (no '-g' nor '-m' option)",
         bytes2mb (hardlimbytes));
  } else msg ("hard memory limit %d MB as specified", bytes2mb (hardlimbytes));
  softlimbytes = (hardlimbytes + 2)/3;
  msg ("soft memory limit of %d MB", bytes2mb (softlimbytes));

  msg ("");
  if (!minclim) {
    minclim = MINCLIM;
    msg ("default minimum conflict limit of %d conflicts", minclim);
  } else
    msg ("minimum conflict limit set to %d ('--min=%d')", minclim, minclim);

  if (!initclim) {
    initclim = INITCLIM;
    msg ("default initial conflict limit of %d conflicts", initclim);
  } else
    msg ("initial conflict limit set to %d ('--init=%d')", 
         initclim, initclim);

  if (!maxclim) {
    maxclim = MAXCLIM;
    msg ("default maximum conflict limit of %d conflicts", maxclim);
  } else
    msg ("maximum conflict limit set to %d ('--max=%d')", maxclim, maxclim);

  msg ("");
  if (optimize < 0) {
    optimize = OPTIMIZE;
    msg ("default optimization level %d", optimize);
  } else
    msg ("optimization level %d as specified ('-O%d')", optimize, optimize);

  msg ("");
  setsighandlers ();

  initroot ();

  msg ("reading %s", fname);
  parse (root);
  if (clf == 1) fclose (file);
  if (clf == 2) pclose (file);

  if (!noparallel) startparallel (root);

  msg ("simplifying root solver instance with optimization level %d", optimize);
  startimer (&wct.simp);
  res = lglsimp (root, optimize);
  sec = stoptimer ();
  js.cnt++, js.simp++;
  propagations += lglgetprops (root);
  assert (!lglgetdecs (root));
  assert (!lglgetconfs (root));
  msg ("root solver instance optimization with result %d took %.2f seconds",
    res, sec);

  tmp = 0;
  if (!noparallel) {
    tmp = joinparallel ();
    if (!res && tmp) parallelwins (tmp);
    releaseparallel ();
  }

  if (!res && tmp) res = tmp;
  else if (res == 10) {
    printf ("s SATISFIABLE\n");
    if (!nowitness) fflush (stdout), witness (root);
    fflush (stdout);
  } else if (res == 20) {
    printf ("s UNSATISFIABLE\n");
    fflush (stdout);
  } else {
    assert (!res);
    node = newnode (0, 0);
    if (!noparallel) startparallel (node->lgl);
    lglsetopt (node->lgl, "block", 0);

    clim = initclim;
    sumclims += clim;

    report ();
    if (!(res = flush ())) {
      search ();
      while (!(res = finish ())) {
	incround ();
	simp ();
	if ((res = flush ())) break;
	lookahead ();
	split ();
	if ((res = flush ())) break;
	report ();
	updateclim ();
	search ();
      }
    }

    if (!noparallel) joinparallel (), releaseparallel ();

    msg ("");
    msg ("cleaning up after %d rounds", round);
    while (numnodes) delnode (nodes[0]);
    while ((leaf = deqleaf ())) deleaf (leaf);
  }

  lglrelease (root);
  resetsighandlers ();

  stats ();
  DEL (jobs, sizejobs);
  DEL (nodes, sizenodes);
  DEL (confstack, sizeconfstack);
  msg ("");
  msg ("result %d", res);

  return res;
}
