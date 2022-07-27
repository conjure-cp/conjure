(set-logic QF_BV)
(declare-fun u0 () (_ BitVec 4))
(declare-fun u1 () (_ BitVec 4))
(assert (= (bvadd (bvshl u0 (bvshl (_ bv1 4) (_ bv0 4))) u0) (bvnot (_ bv0 4))))
(assert (= (bvadd (bvshl u1 (bvshl (_ bv1 4) (_ bv1 4))) u1) (bvnot (_ bv0 4))))
(declare-fun b0 () (_ BitVec 4))
(declare-fun b1 () (_ BitVec 4))
(assert (= (bvxor (bvnot (_ bv0 4)) b0) (bvshl b0 (_ bv1 4))))
(assert (= (bvxor b0 b1) (bvshl b1 (_ bv1 4))))
(assert (not (and (= b0 u0) (= b1 u1))))
(check-sat)
(exit)
