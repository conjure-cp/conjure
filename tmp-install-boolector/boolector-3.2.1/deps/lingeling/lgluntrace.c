/*-------------------------------------------------------------------------*/
/* Copyright 2010-2018 Armin Biere Johannes Kepler University Linz Austria */
/*-------------------------------------------------------------------------*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>
#include <signal.h>

#include "lglib.h"

void lglchkclone (LGL *);

static int verbose, exitonabort, lineno;
static const char * name;

static void die (const char * fmt, ...) {
  va_list ap;
  fputs ("*** lgluntrace: ", stderr);
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
           "*** lgluntrace: parse error in '%s' line %d: ", name, lineno);
  va_start (ap, fmt);
  vfprintf (stderr, fmt, ap);
  va_end (ap);
  fputc ('\n', stderr);
  fflush (stderr);
  exit (1);
}

static void msg (const char * fmt, ...) {
  va_list ap;
  if (!verbose) return;
  fputs ("c [lgluntrace] ", stdout);
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
  if (!(tok = strtok (0, " ")) || !isnumstr (tok) || strtok (0, " ")) {
    perr ("expected integer argument for '%s'", op);
    exit (1);
  }
  return atoi (tok);
}

static int noarg (const char * str, char * op) {
  if (strcmp (str, op)) return 0;
  if (strtok (0, " ")) perr ("argument after '%s'", op);
  return 1;
}

static void exitonsig (int sig) { 
  msg ("exit(%d) on signal %d", sig, sig);
  exit (sig); 
}

int main (int argc, char ** argv) {
  int i, len, ch, close = 0, res, arg;
  char buffer[80], * tok, * opt;
  FILE * file;
  char * cmd;
  LGL * lgl;
  for (i = 1; i < argc; i++) {
    if (!strcmp (argv[i], "-h")) {
      printf ("usage: lgluntrace [-h][-v][-e][<trace>[.gz]]\n");
      exit (0);
    } else if (!strcmp (argv[i], "-v")) verbose = 1;
    else if (!strcmp (argv[i], "-e")) exitonabort = 1;
    else if (argv[i][0] == '-')
      die ("invalid command line option '%s' (try '-h')", argv[i]);
    else if (name)
      die ("two traces '%s' and '%s' specified (try '-h')", name, argv[i]);
    else name = argv[i];
  }
  if(name) {
    len = strlen (name);
    if (len >= 3 && !strcmp (name + len - 3, ".gz")) {
      cmd = malloc (len + 20);
      sprintf (cmd, "gunzip -c %s", name);
      file = popen (cmd, "r");
      free (cmd);
      if (file) close = 2;
    } else {
      file = fopen (name, "r");
      if (file) close = 1;
    }
    if (!file) die ("can not read '%s'", name);
  } else name = "<stdin>", file = stdin;
  if (exitonabort) {
    msg ("setting signal handlers since '-e' specified");
    signal (SIGINT, exitonsig);
    signal (SIGSEGV, exitonsig);
    signal (SIGABRT, exitonsig);
    signal (SIGTERM, exitonsig);
  }
  msg ("reading %s", name);
  buffer[len = 0] = 0;
  lineno = 1;
  res = 0;
  lgl = 0;
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
  msg ("line %d : %s", lineno, buffer);
  if (!(tok = strtok (buffer, " "))) perr ("empty line");
  else if (!strcmp (tok, "add")) lgladd (lgl, intarg ("add"));
  else if (!strcmp (tok, "return")) {
    arg = intarg ("return");
    if (arg != res) 
      die ("expected return value %d but got %d", arg, res);
  } else if (!strcmp (tok, "deref")) res = lglderef (lgl, intarg ("deref"));
  else if (!strcmp (tok, "failed")) res = lglfailed (lgl, intarg ("failed"));
  else if (!strcmp (tok, "fixed")) res = lglfixed (lgl, intarg ("fixed"));
  else if (!strcmp (tok, "repr")) res = lglrepr (lgl, intarg ("repr"));
  else if (noarg (tok, "incvar")) res = lglincvar (lgl);
  else if (noarg (tok, "maxvar")) res = lglmaxvar (lgl);
  else if (noarg (tok, "changed")) res = lglchanged (lgl);
  else if (noarg (tok, "inconsistent")) res = lglinconsistent (lgl);
  else if (noarg (tok, "lkhd")) res = lglookahead (lgl);
  else if (noarg (tok, "fixate")) lglfixate (lgl);
  else if (noarg (tok, "reduce")) lglreducecache (lgl);
  else if (noarg (tok, "flush")) lglflushcache (lgl);
  else if (noarg (tok, "chkclone")) lglchkclone (lgl);
  else if (!strcmp (tok, "assume")) lglassume (lgl, intarg ("assume"));
  else if (noarg (tok, "init")) lgl = lglinit ();
  else if (noarg (tok, "sat")) res = lglsat (lgl);
  else if (!strcmp (tok, "simp")) res = lglsimp (lgl, intarg ("simp"));
  else if (noarg (tok, "stats")) lglstats (lgl);
  else if (!strcmp (tok, "freeze")) lglfreeze (lgl, intarg ("freeze"));
  else if (!strcmp (tok, "melt")) lglmelt (lgl, intarg ("melt"));
  else if (!strcmp (tok, "reuse")) lglreuse (lgl, intarg ("reuse"));
  else if (!strcmp (tok, "frozen")) res = lglfrozen (lgl, intarg ("frozen"));
  else if (!strcmp (tok, "usable")) res = lglusable (lgl, intarg ("usable"));
  else if (!strcmp (tok, "reusable"))
    res = lglreusable (lgl, intarg ("reusable"));
  else if (!strcmp (tok, "setimportant"))
    lglsetimportant (lgl, intarg ("setimportant"));
  else if (noarg (tok, "setphases")) lglsetphases (lgl);
  else if (!strcmp (tok, "setphase")) lglsetphase (lgl, intarg ("setphase"));
  else if (!strcmp (tok, "resetphase"))
    lglresetphase (lgl, intarg ("resetphase"));
  else if (!strcmp (tok, "option")) {
    if (!(opt = strtok (0, " "))) perr ("option name missing");
    lglsetopt (lgl, opt, intarg ("option"));
  } else if (noarg (tok, "release")) lglrelease (lgl);
  else perr ("invalid command '%s'", tok);
  lineno++;
  len = 0;
  goto NEXT;
DONE:
  if(close == 1) fclose (file);
  if(close == 2) pclose (file);
  msg ("done %s", name);
  return 0;
}
