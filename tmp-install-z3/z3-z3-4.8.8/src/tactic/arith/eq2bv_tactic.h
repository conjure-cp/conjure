/*++
Copyright (c) 2015 Microsoft Corporation

Module Name:

    eq2bv_tactic.h

Abstract:

    Extract integer variables that are used as finite domain indicators.
    The integer variables can only occur in equalities.

Author:

    Nikolaj Bjorner (nbjorner) 2015-8-19

Notes:

--*/
#ifndef EQ2BV_TACTIC_H_
#define EQ2BV_TACTIC_H_

#include "util/params.h"
class ast_manager;
class tactic;

tactic * mk_eq2bv_tactic(ast_manager & m);

/*
    ADD_TACTIC("eq2bv", "convert integer variables used as finite domain elements to bit-vectors.", "mk_eq2bv_tactic(m)")
*/


#endif
