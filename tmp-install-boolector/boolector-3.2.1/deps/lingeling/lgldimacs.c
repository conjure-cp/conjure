#include "lgldimacs.h"

#include <assert.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <limits.h>

struct LDR {
  struct { 
    void * state;
    ldralloc alloc;
    ldrdealloc dealloc;
  } mem;
  struct { void * state; ldropt fun; } opt;
  struct { void * state; ldrheader fun; } header;
  struct { void * state; ldradd fun; } add;
  char * path;
  char * errmsg;
  int closefile;
  FILE * file;
  int lineno;
};

static void * ldrstdalloc (void * mem, size_t bytes) {
  (void) mem;
  return malloc (bytes);
}

static void ldrstdealloc (void * mem, void * ptr, size_t bytes) {
  (void) mem;
  (void) bytes;
  free (ptr);
}

static void * ldrstdrealloc (void * mem, void * ptr, size_t ob, size_t nb) {
  (void) mem;
  (void) ob;
  return realloc (ptr, nb);
}

LDR * ldrinit () {
  return ldrminit (0, ldrstdalloc, ldrstdrealloc, ldrstdealloc);
}

LDR * ldrminit (void * state, 
                ldralloc alloc, ldrealloc realloc, ldrdealloc dealloc) {
  LDR * res = alloc (state, sizeof *res);
  if (!res) return res;
  memset (res, 0, sizeof *res);
  res->mem.state = state;
  res->mem.alloc = alloc;
  res->mem.dealloc = dealloc;
  return res;
}

static void ldrdelstr (LDR * ldr, char * str) {
  if (str) ldr->mem.dealloc (ldr->mem.state, str, strlen (str) + 1);
}

static char * ldrstrdup (LDR * ldr, const char* str) {
  size_t bytes = strlen (str) + 1;
  char * res = ldr->mem.alloc (ldr->mem.state, bytes);
  return strcpy (res, str);
}

void ldrelease (LDR * ldr) {
  if (ldr->file) {
    if (ldr->closefile == 1) fclose (ldr->file);
    if (ldr->closefile == 2) pclose (ldr->file);
  }
  ldrdelstr (ldr, ldr->errmsg);
  ldrdelstr (ldr, ldr->path);
  ldr->mem.dealloc (ldr->mem.state, ldr, sizeof *ldr);
}

void ldrsetopt (LDR * ldr, void * state, ldropt fun) {
  ldr->opt.fun = fun;
  ldr->opt.state = state;
}

void ldrsetheader (LDR * ldr, void * state, ldrheader fun) {
  ldr->header.fun = fun;
  ldr->header.state = state;
}

void ldrsetadd (LDR * ldr, void * state, ldradd fun) {
  ldr->add.fun = fun;
  ldr->add.state = state;
}

static int ldrfilexists (const char * path) {
  struct stat buf;
  return !stat (path, &buf);
}

static int ldrperr (LDR * ldr, const char * msg) {
  size_t bytes, len;
  char * str;
  assert (!ldr->errmsg);
  assert (ldr->path);
  bytes = strlen (msg) + strlen (ldr->path) + 20;
  str = ldr->mem.alloc (ldr->mem.state, bytes);
  sprintf (str, "%s:%d: %s", ldr->path, ldr->lineno, msg);
  len = strlen (str) + 1;
  ldr->errmsg = strcpy (ldr->mem.alloc (ldr->mem.state, len), str);
  ldr->mem.dealloc (ldr->mem.state, str, bytes);
  return 0;
}

static int ldrhas (const char * str, const char * suffix) {
  int l = strlen (str), k = strlen (suffix);
  if (l < k) return 0;
  return !strcmp (str + l - k, suffix);
}

static FILE * ldrcmd (LDR * ldr, const char * fmt, const char * name) {
  FILE * res;
  int len = strlen (fmt) + strlen (name) + 1;
  char * s = ldr->mem.alloc (ldr->mem.state, len);
  sprintf (s, fmt, name);
  res = popen (s, "r");
  ldr->mem.dealloc (ldr->mem.state, s, len);
  return res;
}

void ldrsetpath (LDR * ldr, const char * path) {
  assert (!ldr->file);
  assert (!ldr->path);
  assert (!ldr->closefile);
  ldr->path = ldrstrdup (ldr, path);
  if (!ldrfilexists (path))
    return (void) ldrperr (ldr, "file does not exist");
  ldr->closefile = 2;
  if (ldrhas (path, ".gz"))
    ldr->file = ldrcmd  (ldr, "gunzip -c %s", path);
  else if (ldrhas (path, ".bz2"))
    ldr->file = ldrcmd (ldr, "bzcat %s", path);
  else if (ldrhas (path, ".7z"))
    ldr->file = ldrcmd (ldr, "7z x -so %s 2>/dev/null", path);
  else if (ldrhas (path, ".lzma"))
    ldr->file = ldrcmd (ldr, "lzcat %s", path);
  else ldr->file = fopen (path, "r"), ldr->closefile = 1;
  if (!ldr->file) return (void) ldrperr (ldr, "can not open file");
}

void ldrsetfile (LDR * ldr, FILE * file) {
  assert (!ldr->file);
  assert (!ldr->path);
  assert (!ldr->closefile);
  ldr->file = file;
  ldr->path = ldrstrdup (ldr, "<unspecified-path>");
}

void ldrsetnamedfile (LDR * ldr, FILE * file, const char * path) {
  assert (!ldr->file);
  assert (!ldr->path);
  assert (!ldr->closefile);
  ldr->file = file;
  ldr->path = ldrstrdup (ldr, path);
}

const char * ldrerr (LDR * ldr) { return ldr->errmsg; }

static int ldrnext (LDR * ldr) {
  int ch;
  assert (ldr);
  assert (ldr->file);
  ch = getc (ldr->file);
  if (ch == '\n') ldr->lineno++;
  return ch;
}

int ldrparse (LDR * ldr) {
  struct { int parsed, specified; } vars, clauses;
  int ch, sign, lit, digit;
  if (ldr->errmsg) return 0;
  while ((ch = ldrnext (ldr)) == 'c') {
    // TODO parse embedded options
    while ((ch = ldrnext (ldr)) != '\n')
      if (ch == EOF)
	return ldrperr (ldr, "end-of-file in comment before header");
  }
  if (ch != 'p') return ldrperr (ldr, "expected 'p' or 'c'");
  if (ldrnext (ldr) != ' ') return ldrperr (ldr, "expected space after 'p'");
  if (ldrnext (ldr) != 'c') return ldrperr (ldr, "expected 'c' after 'p '");
  if (ldrnext (ldr) != 'n') return ldrperr (ldr, "expected 'n' after 'p c'");
  if (ldrnext (ldr) != 'f') return ldrperr (ldr, "expected 'f' after 'p cn'");
  if (ldrnext (ldr) != ' ')
    return ldrperr (ldr, "expected space after 'p cnf'");
  ch = ldrnext (ldr);
  if (!isdigit (ch)) return ldrperr (ldr, "expected digit after 'p cnf '");
  vars.specified = ch - '0';
  while (isdigit (ch = ldrnext (ldr))) {
    if (INT_MAX/10 < vars.specified)
NUMBER_TOO_LARGE:
      return ldrperr (ldr, "number too large");
    vars.specified *= 10;
    digit = (ch - '0');
    if (INT_MAX - digit < vars.specified) goto NUMBER_TOO_LARGE;
    vars.specified += digit;
  }
  if (ch != ' ')
    return ldrperr (ldr, "expected space after maximum variable index");
  if (!isdigit (ch = ldrnext (ldr)))
    return ldrperr (ldr, "expected digit after space after variable index");
  clauses.specified = ch - '0';
  while (isdigit (ch = ldrnext (ldr))) {
    if (INT_MAX/10 < clauses.specified) goto NUMBER_TOO_LARGE;
    clauses.specified *= 10;
    digit = (ch - '0');
    if (INT_MAX - digit < clauses.specified) goto NUMBER_TOO_LARGE;
    clauses.specified += digit;
  }
  while (ch == ' ' || ch == '\t' || ch == '\r')
    ch = ldrnext (ldr);
  if (ch != '\n') return ldrperr (ldr, "expected new line after header");
  if (ldr->header.fun)
    ldr->header.fun (ldr->header.state, vars.specified, clauses.specified);
  vars.parsed = clauses.parsed = 0;
  lit = 0;
  for (;;) {
    ch = ldrnext (ldr);
    if (ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n') continue;
    if (ch == 'c') {
      while ((ch = ldrnext (ldr)) != '\n') {
	if (ch == EOF)
	  return ldrperr (ldr, "end-of-file in comment after header");
      }
      continue;
    }
    if (ch == EOF) {
      if (lit) return ldrperr (ldr, "zero sentinel missing at end-of-file");
      assert (clauses.parsed <= clauses.specified);
      if (clauses.parsed + 1 == clauses.specified)
        return ldrperr (ldr, "one clause is missing");
      if (clauses.parsed < clauses.specified)
        return ldrperr (ldr, "clauses are missing");
      break;
    }
    if (ch == '-') {
      ch = ldrnext (ldr);
      if (!isdigit (ch)) return ldrperr (ldr, "expected digit after '-'");
      sign = -1;
    } else if (!isdigit (ch)) return ldrperr (ldr, "expected digit or '-'");
    else sign = 1;
    assert (clauses.parsed <= clauses.specified);
    if (clauses.specified == clauses.parsed)
      return ldrperr (ldr, "too many clauses");
    lit = ch - '0';
    while (isdigit (ch = ldrnext (ldr))) {
      if (INT_MAX/10 < lit) goto NUMBER_TOO_LARGE;
      lit *= 10;
      digit = (ch - '0');
      if (INT_MAX - digit < lit) goto NUMBER_TOO_LARGE;
      lit += digit;
    }
    assert (0 <= lit);
    if (lit > vars.specified)
      return ldrperr (ldr, "maximum variable index exceeded");
    lit *= sign;
    assert ((sign < 0) == (lit < 0));
    if (ldr->add.fun) ldr->add.fun (ldr->add.state, lit);
    if (lit) continue;
    clauses.parsed++;
    assert (clauses.parsed <= clauses.specified);
  }
  return 1;
}
