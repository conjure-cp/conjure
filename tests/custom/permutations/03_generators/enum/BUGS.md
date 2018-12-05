There appears to be a bug in refinement
- 0001
- 0002
are affected 
These examples both work with ints instead of enums

A minimal bug example is in this folder - the problem is sets of tuples of enums

$minimal bug example
  letting n be new type enum {E1,E2,E3,E4} 
  letting i be (E1,E2)
  find s : set of (n,n)
  such that i in s 


IO Error
Type error in 4 * 4
              The argument has type: matrix indexed by [int] of int

CallStack (from HasCallStack):
  error, called at src/Conjure/Bug.hs:21:15 in conjure-cp-2.2.0-4cfnInyB42NJSP2i6f0krZ:Conjure.Bug
  bug, called at src/Conjure/Bug.hs:47:16 in conjure-cp-2.2.0-4cfnInyB42NJSP2i6f0krZ:Conjure.Bug
