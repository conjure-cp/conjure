(set-logic QF_AUFBV)
(declare-fun a1 () (Array (_ BitVec 2) (_ BitVec 1)))
(define-fun $e2 () (_ BitVec 2) (_ bv0 2))
(define-fun $e3 () (_ BitVec 2) (_ bv2 2))
(define-fun $e4 () (_ BitVec 1) (select a1 $e2))
(define-fun $e5 () (_ BitVec 1) (select a1 (bvnot $e3)))
(define-fun $e6 () (_ BitVec 1) (select a1 $e3))
(define-fun $e7 () (_ BitVec 1) (select a1 (bvnot $e2)))
(assert (not (= $e6 #b0)))
(assert (not (= (bvnot $e7) #b0)))
(assert (not (= $e4 #b0)))
(assert (not (= $e5 #b0)))
(check-sat)
(exit)
