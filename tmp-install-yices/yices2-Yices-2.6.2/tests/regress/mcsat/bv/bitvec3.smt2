(set-logic QF_BV)
(declare-fun _substvar_102_ () (_ BitVec 32))
(declare-fun _substvar_93_ () (_ BitVec 32))
(declare-fun _substvar_333_ () (_ BitVec 3))
(declare-fun _substvar_339_ () (_ BitVec 2))
(declare-fun _substvar_326_ () (_ BitVec 3))
(assert (let ( (?v_2 ((_ extract 2 0) _substvar_102_))) (let ( (?v_8 ((_ extract 1 1) _substvar_102_))) (let ( (?v_12 (= ?v_8 (_ bv1 1)))) (let ( (?v_17 ((_ extract 2 2) _substvar_102_))) (let ( (?v_21 (= ?v_17 (_ bv1 1)))) (not (=> (and (= ((_ extract 1 0) _substvar_93_) (_ bv3 2)) (= _substvar_339_ (_ bv3 2))) (=> (and (and (and (ite (= ((_ extract 0 0) _substvar_93_) (_ bv1 1)) (= _substvar_326_ _substvar_333_) (= _substvar_326_ (_ bv0 3))) (ite (= ((_ extract 1 1) _substvar_93_) (_ bv1 1)) (= ?v_2 (concat _substvar_339_ (_ bv0 1))) (= ?v_2 (_ bv0 3)))) (= (_ bv0 1) (ite ?v_12 (_ bv1 1) (_ bv0 1)))) (= (_ bv0 1) (ite ?v_21 (_ bv1 1) (_ bv0 1)))) false)))))))))
(check-sat)
(exit)
