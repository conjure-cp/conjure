#ifndef LGLDIMACS_h_INCLUDED
#define LGLDIMACS_h_INCLUDED

#include <stdlib.h>
#include <stdio.h>

typedef struct LDR LDR;

typedef void * (*ldralloc) (void* state, size_t);
typedef void * (*ldrealloc) (void* state, void*, size_t, size_t);
typedef void (*ldrdealloc) (void* state, void*, size_t);
typedef void (*ldropt)(void *state, const char * opt, int val);
typedef void (*ldrheader)(void *state, int vars, int clauses);
typedef void (*ldradd)(void *state, int lit);

LDR * ldrinit ();
LDR * ldrminit (void *  state, ldralloc, ldrealloc, ldrdealloc);
void ldrelease (LDR *);

void ldrsetopt (LDR *, void * optmgr, ldropt);
void ldrsetheader (LDR *, void * header, ldrheader);
void ldrsetadd (LDR *, void * adder, ldradd);

void ldrsetpath (LDR *, const char * path);
void ldrsetfile (LDR *, FILE * file);
void ldrsetnamedfile (LDR *, FILE * file, const char * path);

int ldrparse (LDR *);

const char * ldrerr (LDR *);

#endif
