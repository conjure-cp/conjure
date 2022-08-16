/*-------------------------------------------------------------------------*/
/* Copyright 2010-2018 Armin Biere Johannes Kepler University Linz Austria */
/*-------------------------------------------------------------------------*/

#include "lglib.h"

#ifdef NDEBUG
#undef NDEBUG
#endif

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/times.h>
#include <sys/wait.h>
#include <ctype.h>
#include <string.h>
#include <limits.h>
#include <stdarg.h>
#include <fcntl.h>
#include <signal.h>
#include <sys/time.h>

void lglchkclone (LGL *);

typedef struct Data { 
  LGL * lgl; 
  FILE * trace;
  int * available, navailable, * frozen, nfrozen;
  int m, n, c, print, noptsfuzz;
} Data;

typedef void * (*State)(Data *, unsigned rand);

typedef struct Event {
  State state;
  unsigned rand;
  int remove;
} Event;

typedef struct RNG { unsigned z, w; } RNG;

typedef struct Env { 
  FILE * file;
  const char * prefix;
  int round, bugs, violations, print, quiet, first, nodd, noptsfuzz;
  int alwaysfork, terminal, forked, nevents, timeout;
  unsigned seed;
  Event * events;
  RNG rng;
} Env;

static void * init (Data *, unsigned);
static void * opts (Data *, unsigned);
static void * inc (Data *, unsigned);
static void * cnf (Data *, unsigned);
static void * unit (Data *, unsigned);
static void * binary (Data *, unsigned);
static void * ternary (Data *, unsigned);
static void * rest (Data *, unsigned);
static void * clause (Data *, unsigned);
static void * lkhd (Data *, unsigned);
static void * sat (Data *, unsigned);
static void * release (Data *, unsigned);

static RNG initrng (unsigned seed) {
  RNG res;
  res.z = seed * 1000632769u;
  res.w = seed * 2019164533u;
  return res;
}

static unsigned nextrand (RNG * rng) {
  rng->z = 36969 * (rng->z & 65535) + (rng->z >> 16);
  rng->w = 18000 * (rng->w & 65535) + (rng->w >> 16);
  return (rng->z << 16) + rng->w;  /* 32-bit result */
}

static int pick (RNG * rng_ptr, unsigned from, unsigned to) {
  unsigned tmp = nextrand (rng_ptr);
  int res;
  assert (from <= to && to <= INT_MAX);
  tmp %= to - from + 1;
  tmp += from;
  res = (int) tmp;
  // printf ("pick %u %u %d\n", from, to, res);
  return res;
}

static void onabort (void * dummy) { (void) dummy; exit (42); }

static void * init (Data * data, unsigned r) {
  RNG rng = initrng (r);
  int t, i;
  data->m = pick (&rng, 10, 200);
  t = pick (&rng, 390, 450);
  data->n = (data->m * t) / 100;
  if (data->print) {
    printf ("cnf %d %d ", data->m, data->n);
    fflush (stdout);
  }
  data->lgl = lglinit ();
  lglonabort (data->lgl, 0, onabort);
  if (data->trace) lglwtrapi (data->lgl, data->trace);
  data->navailable = data->m;
  data->available = malloc (data->navailable * sizeof *data->available);
  for (i = 0; i < data->navailable; i++) data->available[i] = i + 1;
  if (data->noptsfuzz) return cnf;
  return  (pick (&rng, 0, 1)) ? opts : cnf;
}

static void * inc (Data * data, unsigned r) {
  int i, oldavailable;
  int newvars;
  RNG rng;
  rng = initrng (r);
  if (pick (&rng, 0, 1)) lglmaxvar (data->lgl);
  while (pick (&rng, 0, 4))
    lglfixed (data->lgl, pick (&rng, 1, data->m));
  while (pick (&rng, 0, 3))
    lglrepr (data->lgl, pick (&rng, 1, data->m));
  if ((newvars = pick (&rng, 0, 50))) {
    if (pick (&rng, 0, 1)) lglincvar (data->lgl);
    oldavailable = data->navailable;
    data->navailable += newvars;
    data->available = 
      realloc (data->available, data->navailable * sizeof *data->available);
    for (i = 0; i < newvars; i++)
      data->available[oldavailable + i] = data->m + i + 1;
    data->m += newvars;
  }
  return cnf;
}

static void * opts (Data * data, unsigned r) {
  int n = 0, min, val, max, m;
  void * it;
  RNG rng = initrng (r);
  const char * name;
  if (!pick (&rng, 0, 10)) lglsetopt (data->lgl, "plain", 1);
  else {
    it = lglfirstopt (data->lgl); 
    while ((it = lglnextopt (data->lgl, it, &name, &val, &min, &max)))
      n++;
    assert (n > 0);
    m = pick (&rng, 1, 10);
    it = lglfirstopt (data->lgl);
    while ((it = lglnextopt (data->lgl, it, &name, &val, &min, &max))) {
      if (pick (&rng, 1, m) != 1) continue;
      if (!strcmp (name, "check")) continue;
      if (!strcmp (name, "drupligtrace")) continue;
      if (!strcmp (name, "log")) continue;
      if (!strcmp (name, "sleeponabort")) continue;
      if (!strcmp (name, "exitonabort")) continue;
      if (!strcmp (name, "verbose")) continue;
      if (!strcmp (name, "witness")) continue;
      if (!strcmp (name, "prune")) continue;
      if (pick (&rng, 0, 1)) {
	if (!strcmp (name, "locsmaxeff")) max = 10*min;
	if (!strcmp (name, "locsrtc")) max = 0;
	while (pick (&rng, 0, 1) && val < max) {
	  if (val > INT_MAX/2) break;
	  if (val < 4) val++; else val *= 2;
	}
	if (val > max) val = max;
      } else {
	while (pick (&rng, 0, 1) && val > min) {
	  if (val > 0) val /= 2;
	  else if (val > -4) val--;
	  else if (val < INT_MIN/2) break;
	  else val *= 2;
	}
	if (val < min) val = min;
      }
      lglsetopt (data->lgl, name, val);
    }
  }
#ifndef NLGLDRUPLIG
  if (!pick (&rng, 0, 3)) {
    lglsetopt (data->lgl, "druplig", 1);
    lglsetopt (data->lgl, "drupligcheck", 1);
  }
#endif
  return cnf;
}

static void * cnf (Data * data, unsigned r) {
  (void) r;
  return data->c < data->n ? unit : lkhd;
}

static int lit (Data * data, RNG * r) {
  int pos = pick (r, 0, data->navailable - 1);
  int res = data->available[pos];
  assert (0 < res && res <= data->m);
  if (pick (r, 0, 1)) res = -res;
  return res;
}

static void * unit (Data * data, unsigned r) {
  RNG rng = initrng (r);
  lgladd (data->lgl, lit (data, &rng));
  if (!pick (&rng, 0, 99)) return clause;
  return binary;
}

static void * binary (Data * data, unsigned r) {
  RNG rng = initrng (r);
  lgladd (data->lgl, lit (data, &rng));
  if (!pick (&rng, 0, 10)) return clause;
  return ternary;
}

static void * ternary (Data * data, unsigned r) {
  RNG rng = initrng (r);
  lgladd (data->lgl, lit (data, &rng));
  if (pick (&rng, 0, 2)) return clause;
  return rest;
}

static void * rest (Data * data, unsigned r) {
  RNG rng = initrng (r);
  lgladd (data->lgl, lit (data, &rng));
  if (pick (&rng, 0, 3)) return clause;
  return rest;
}

static void * clause (Data * data, unsigned r) {
  lgladd (data->lgl, 0);
  data->c += 1;
  return cnf;
}

static int gcd (int a, int b) {
  int r;
  assert (a > 0 && b > 0);
  while (b > 0) {
    r = a % b;
    a = b;
    b = r;
  }
  return a;
}

static void * lkhd (Data * data, unsigned r) {
  RNG rng = initrng (r);
  if (!pick (&rng, 0, 10)) (void) lglookahead (data->lgl);
  return sat;
}

static void * sat (Data * data, unsigned r) {
  int res, freeze, i, pos, delta, lit, * assumed, nassumed, szassumed;
  LGL * lgl = data->lgl;
  State next = release;
  RNG rng;
  rng = initrng (r);
  if (!pick (&rng, 0, 500)) lglchkclone (lgl);
  freeze = pick (&rng, 0, 10);
  if (freeze) {
    if (data->navailable > 1) {
      data->nfrozen =
        pick (&rng, (data->navailable+9)/10, 2*(data->navailable+2)/3);
      data->frozen = malloc (data->nfrozen * sizeof *data->frozen);
      delta = pick (&rng, 1, data->navailable-1);
      while (gcd (data->navailable, delta) != 1)
	if (++delta == data->navailable) delta = 1;
      pos = pick (&rng, 0, data->navailable-1);
      for (i = 0; i < data->nfrozen; i++) {
	assert (0 <= pos && pos < data->navailable);
	lit = data->available[pos];
	data->frozen[i] = lit;
	pos += delta;
	if (pos >= data->navailable) pos -= data->navailable;
      }
      for (i = 0; i < data->nfrozen; i++)
	lglfreeze (lgl, data->frozen[i]);
    } else if (data->navailable == 1) {
      data->nfrozen = 1;
      data->frozen = malloc (sizeof *data->frozen);
      data->frozen[0] = data->available[0];
    } else {
      data->nfrozen = 0;
      data->frozen = 0;
    }
  }
#if 0
  if (data->navailable && !pick (&rng, 0, 10)) {
    i = pick (&rng, 0, 20);
    while (i-- > 0) {
      pos = pick (&rng, 0, data->navailable - 1);
      lit = data->available[pos];
      if (pick (&rng, 0, 1)) lit = -lit;
      lglcassume (lgl, lit);
    }
    lglcassume (lgl, 0);
  }
#endif
  if (data->navailable && !pick (&rng, 0, 3)) {
    nassumed = 0;
    assumed = malloc ((szassumed = 1)*sizeof *assumed);
    do {
      if (nassumed == szassumed)
	assumed = realloc (assumed, (szassumed *= 2)*sizeof *assumed);
      pos = pick (&rng, 0, data->navailable - 1);
      lit = data->available[pos];
      if (pick (&rng, 0, 1)) lit = -lit;
      lglassume (lgl, lit);
      assert (nassumed < szassumed);
      assumed[nassumed++] = lit;
    } while (!pick (&rng, 0, 10));
  } else assumed = 0, nassumed = szassumed = 0;
  if (!pick (&rng, 0, 4)) {
    pos = pick (&rng, 0, data->navailable - 1);
    lit = data->available[pos];
    if (pick (&rng, 0, 1)) lit = -lit;
    if (!pick (&rng, 0, 3)) lglresetphase (lgl, lit);
    else lglsetphase (lgl, lit);
    if (!pick (&rng, 0, 11)) lglsetimportant (lgl, lit);
  }
  if (!pick (&rng, 0, 100)) lglchkclone (lgl);
  if (!pick (&rng, 0, 66)) lglfixate (lgl);
  if (pick (&rng, 0, 20)) res = lglsat (lgl);
  else res = lglsimp (lgl, pick (&rng, 0, 10));
  assert (!res || res == 10 || res == 20);
  if (res == 10) {
    if (data->print) printf ("sat ");
    if (!pick (&rng, 0, 4)) lglinconsistent (lgl);
    if (!pick (&rng, 0, 20)) lglsetphases (lgl);
    i = pick (&rng, 0, data->m);
    while (i--) {
      lit = pick (&rng, 1, 2*data->m);
      if (pick (&rng, 0, 1)) lit = -lit;
      lglderef (lgl, lit);
    }
    if (!pick (&rng, 0, 30)) lglsetphases (lgl);
    if (freeze) {
      assert (data->nfrozen <= data->navailable);
      data->navailable = 0;
      for (i = 0; i < data->nfrozen; i++) {
	lit = data->frozen[i];
	switch (i % 5) {
	  case 0:
	    lglmelt (lgl, lit);
	    /* FALL THROUGH */
	  case 1:
	    data->available[data->navailable] = lit;
	    data->navailable++;
	    break;
	  case 2:
	    data->available[data->navailable] = ++data->m;
	    data->navailable++;
	    break;
	  case 3:
	    lglmelt (lgl, lit);
	    /* FALL THROUGH */
	    break;
	  default:
	    break;
	}
      }
      free (data->frozen);
    }
    if (freeze >= 2) {
      data->n = (pick (&rng, 101, 130) * data->n + 99) / 100;
      next = inc;
    }
    if (!pick (&rng, 0, 4)) lglchanged (lgl);
    if (!pick (&rng, 0, 3)) {
      int count = pick (&rng, 1, (data->m)/10), i;
      for (i = 0; i < count; i++) {
	int lit = pick (&rng, 1, data->m);
	if (!lglusable (lgl, lit) && lglreusable (lgl, lit))
	  lglreuse (lgl, lit);
      }
    }
  } else if (res == 20) {
    if (data->print) printf ("uns ");
    if (nassumed > 0) {
      i = pick (&rng, 0, 3*nassumed/2);
      while (i-- > 0)
	(void) lglfailed (data->lgl, assumed [ pick (&rng, 0, nassumed-1)]);
    }
    if (!pick (&rng, 0, 4)) lglinconsistent (lgl);
  } else if (data->print) printf ("nil ");
  if (data->print) fflush (stdout);
  if (!pick (&rng, 0, 11)) lglreducecache (lgl);
  else if (!pick (&rng, 0, 17)) lglflushcache (lgl);
  if (!pick (&rng, 0, 1000)) lglchkclone (lgl);
  free (assumed);
  return next;
}

static void * release (Data * data, unsigned r) {
  lglrelease (data->lgl);
  return 0;
}

static void rantrav (Env * env) {
  State state, next;
  unsigned rand;
  Data data;
  memset (&data, 0, sizeof data);
  data.print = env->print;
  env->rng.z = env->rng.w = env->seed;
  for (state = init; state; state = next) {
    rand = nextrand (&env->rng);
    if (env->file) { 
      fprintf (env->file, "%p %x\n", state, rand);
      fflush  (env->file);
    }
    next = state (&data, rand);
  }
}

static void erase (void) {
  int i;
  fputc ('\r', stdout);
  for (i = 0; i < 79; i++) fputc (' ', stdout);
  fputc ('\r', stdout);
}

static int isnumstr (const char * str) {
  const char * p;
  for (p = str; *p; p++)
    if (!isdigit ((int)*p)) return 0;
  return 1;
}

static void die (const char * msg, ...) {
  va_list ap;
  fputs ("*** lglmbt: ", stderr);
  va_start (ap, msg);
  vfprintf (stderr, msg, ap);
  va_end (ap);
  fputc ('\n', stderr);
  fflush (stderr);
  exit (1);
}

static int run (Env * env, void(*process)(Env *)) {
  int res, status, saved1, saved2, null, tmp;
  pid_t id;
  env->forked++;
  fflush (stdout);
  if ((id = fork ())) {
#ifndef NDEBUG
    pid_t wid =
#endif
    wait (&status);
    assert (wid == id);
  } else { 
    saved1 = dup (1);
    saved2 = dup (2);
    null = open ("/dev/null", O_WRONLY);
    close (1);
    close (2);
#ifndef NDEBUG
    tmp = 
#endif
    dup (null);
    assert (tmp == 1);
#ifndef NDEBUG
    tmp = 
#endif
    dup (null);
    assert (tmp == 2);
    if (env->timeout > 0) alarm (env->timeout);
    process (env); 
    close (null);
    close (2);
#ifndef NDEBUG
    tmp = 
#endif
    dup (saved2);
    assert (tmp == 2);
    close (1);
#ifndef NDEBUG
    tmp = 
#endif
    dup (saved1);
    assert (tmp == 1);
    exit (0); 
  }
  if (WIFEXITED (status)) {
    res = WEXITSTATUS (status);
    if (res == 42) res = 0, env->violations++;
    else if (env->print) printf ("exit %d ", res);
  } else if (WIFSIGNALED (status)) {
    if (env->print) printf ("signal");
    res = 1;
  } else {
    if (env->print) printf ("unknown");
    res = 1;
  }
  return res;
}

static void printrace (Env * env) {
  char * name;
  Data data;
  Event * e;
  int i;
  memset (&data, 0, sizeof data);
  data.print = 0;
  assert (env->events);
  assert (env->prefix);
  name = malloc (strlen (env->prefix) + 80);
  sprintf (name, "%s-%u.trace", env->prefix, env->seed);
  data.trace = fopen (name, "w");
  data.noptsfuzz = env->noptsfuzz;
  assert (data.trace); //FIXME
  for (i = 0; i < env->nevents; i++) {
    e = env->events + i;
    if (e->remove) continue;
    (void) e->state (&data, e->rand);
  }
  fclose (data.trace);
  free (name);
}

static void prwc (Env * env, const char * prefix) {
  char * name = malloc (strlen (prefix) + 80);
  int ch, res = 0;
  FILE * file;
  sprintf (name, "%s-%u.trace", prefix, env->seed);
  file = fopen (name, "r");
  assert (file);//FIXME
  while ((ch = getc (file)) != EOF)
    if (ch == '\n') res++;
  fclose (file);
  if (!env->quiet) {
    printf (" %s %d", name, res);
    fflush (stdout);
  }
  free (name);
}

static void dd (Env * env, const char * filename, int golden, int opt) {
  unsigned rand;
  State state;
  FILE * file;
  char * cmd;
  int i;
  file = fopen (filename, "r");
  assert (file);//FIXME
  env->nevents = 0;
  while (fscanf (file, "%p %x\n", &state, &rand) == 2)
    env->nevents += 1;
  fclose (file);
  env->events = calloc (env->nevents, sizeof (Event));
  file = fopen (filename, "r");
  assert (file);//FIXME
  i = 0;
  while (fscanf (file, "%p %x\n", &state, &rand) == 2) {
    assert (i < env->nevents);
    env->events[i].state = state;
    env->events[i].rand = rand;
    env->events[i].remove = 0;
    i++;
  }
  fclose (file);
  assert (i == env->nevents);
  env->prefix = "bug";
  run (env, printrace);
  prwc (env, "bug");
  // TODO now do delta debugging on the model level
  // ...
  cmd = malloc (100);
  if (env->nodd)
    sprintf (cmd, 
	     "cp bug-%u.trace red-%u.trace",
	     env->seed, env->seed);
  else
    sprintf (cmd, 
	     "./lglddtrace %s bug-%u.trace red-%u.trace",
	     opt ? "-O" : "", env->seed, env->seed);
  {
    int tmp = system (cmd);
    (void) tmp;
  }
  prwc (env, "red");
  free (cmd);
  free (env->events);
}

static unsigned hashmac (void) {
  FILE * file = fopen ("/sys/class/net/eth0/address", "r");
  unsigned mac[6], res = 0;
  if (!file) return 0;
  if (fscanf (file, "%02x:%02x:%02x:%02x:%02x:%02x",
              mac+0, mac+1, mac+2, mac+3, mac+4, mac+5) == 6) {
    res =  mac[5];
    res ^= mac[4] << 4;
    res ^= mac[3] << 8;
    res ^= mac[2] << 16;
    res ^= mac[1] << 20;
    res ^= mac[0] << 24;
  }
  fclose (file);
  return res;
}

static const char * usage =
"usage: lglmbt [ <option> ... ] [ <seed> ]\n"
"\n"
"where <option> is one of the following:\n"
"\n"
"  -k | --keep-lines\n"
"  -q | --quiet\n"
"  -f | --first-bug-only\n"
"  -n | --no-delta-debugging\n"
"  -a | --always-fork\n"
"  -O | --optimize-by-delta-debugging-options\n"
"\n"
"  -m <maxruns>\n"
;

static Env env;
static double start;

static double currentime (void) {
  double res = 0;
  struct timeval tv;
  if (!gettimeofday (&tv, 0)) res = 1e-6 * tv.tv_usec, res += tv.tv_sec;
  return res;
}

static double getime () { return currentime () - start; }
static double average (double a, double b) { return b ? a / b : 0; }

static void stats (void) {
  double t = getime ();
  int valid = env.round - env.violations;
  printf ("[lglmbt] finished after %.2f seconds\n", t);
  printf ("[lglmbt] %d rounds = %.0f rounds per second\n", 
    env.round, average (env.round, t));
  printf ("[lglmbt] %d violations = %.0f rounds per second\n", 
    env.violations, average (env.violations, t));
  printf ("[lglmbt] %d valid runs = %.0f rounds per second\n", 
    valid, average (valid, t));
  printf ("[lglmbt] %d bugs = %.0f bugs per second\n", 
    env.bugs, average (env.bugs, t));
}

static void sighandler (int sig) {
  fflush (stdout);
  fflush (stderr);
  printf ("*** lglmbt: caught signal %d in round %d\n", sig, env.round);
  fflush (stdout);
  stats ();
  exit (1);
}

static void setsighandlers (void) {
  (void) signal (SIGINT, sighandler);
  (void) signal (SIGSEGV, sighandler);
  (void) signal (SIGABRT, sighandler);
  (void) signal (SIGTERM, sighandler);
}

int main (int argc, char ** argv) {
  int i, max, res, tmp, prev, opt, mac, pid;
  start = currentime ();
  memset (&env, 0, sizeof env);
  max = INT_MAX; 
  prev = 1;
  memset (&env, 0, sizeof env);
  env.seed = -1;
  env.terminal = isatty (1);
  env.timeout = 0;
  opt = 0;
  for (i = 1; i < argc; i++) {
    if (!strcmp (argv[i], "-h")) {
      printf ("%s", usage);
      exit (0);
    }
    else if (!strcmp (argv[i], "-k") ||
             !strcmp (argv[i], "--keep-lines")) env.terminal = 0;
    else if (!strcmp (argv[i], "-q") ||
             !strcmp (argv[i], "--quiet")) env.quiet = 1;
    else if (!strcmp (argv[i], "-f") ||
             !strcmp (argv[i], "--first-bug-only")) env.first = 1;
    else if (!strcmp (argv[i], "-n") ||
             !strcmp (argv[i], "--no-delta-debugging")) env.nodd = 1;
    else if (!strcmp (argv[i], "-d") ||
             !strcmp (argv[i], "--do-not-fuzz-options")) env.noptsfuzz = 1;
    else if (!strcmp (argv[i], "-a") ||
             !strcmp (argv[i], "--always-fork")) env.alwaysfork = 1;
    else if (!strcmp (argv[i], "-O") ||
             !strcmp (argv[i], "--optimize-by-delta-debugging-options"))
      opt = 1;
    else if (!strcmp (argv[i], "-t")) {
      if (++i == argc)
	die ("argument to '-t' missing (try '-h')");
      if (!isnumstr (argv[i]))
	die ("argument '%s' to '-t' not a number (try '-h')", argv[i]);
      env.timeout = atoi (argv[i]);
    } else if (!strcmp (argv[i], "-m")) {
      if (++i == argc)
	die ("argument to '-m' missing (try '-h')");
      if (!isnumstr (argv[i]))
	die ("argument '%s' to '-m' not a number (try '-h')", argv[i]);
      max = atoi (argv[i]);
    } else if (!isnumstr (argv[i])) {
      die ("invalid command line option '%s' (try '-h')", argv[i]);
    } else env.seed = atoi (argv[i]);
  }
  env.print = !env.quiet;
  if (env.seed != -1 && !env.alwaysfork) {
    rantrav (&env);
    printf ("\n");
  } else {
    mac = hashmac ();
    pid = getpid ();
    setsighandlers ();
    for (env.round = 0; env.round < max; env.round++) {
      if (!(prev & 1)) prev++;
      {
	env.seed = mac;
	env.seed *= 123301093;
	env.seed += times (0);
	env.seed *= 223531513;
	env.seed += pid;
	env.seed *= 31752023;
	env.seed += prev;
	env.seed *= 43376579;
	prev = env.seed = abs (env.seed) >> 1;
      }
      if (!env.quiet) {
	if (env.terminal) erase ();
	printf ("%d %d ", env.round, env.seed);
	fflush (stdout);
      }
      res = run (&env, rantrav);
      if (res > 0) {
	char name[100];
	env.bugs++;
	sprintf (name, "/tmp/lglmbt-tmp-%d.trace", getpid ());
	env.file = fopen (name, "w");
	env.print = 0;
#ifndef NDEBUG
	tmp = 
#endif
	run (&env, rantrav);
	assert (tmp == res);
	fclose (env.file);
	env.file = 0;
	dd (&env, name, res, opt);
	unlink (name);
	env.print = !env.quiet;
      }
      if (!env.quiet) {
	if (res || !env.terminal) printf ("\n");
	fflush (stdout);
      }
      if (res && env.first) break;
    }
  }
  if (!env.quiet) {
    if (env.terminal) erase ();
    printf ("forked %d\n", env.forked);
  }
  stats ();
  return 0;
}
