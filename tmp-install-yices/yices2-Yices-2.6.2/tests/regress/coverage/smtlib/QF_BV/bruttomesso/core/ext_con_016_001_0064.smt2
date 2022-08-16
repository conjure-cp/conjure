(set-logic QF_BV)
(set-info :source | Generated by Roberto Bruttomesso |)
(set-info :smt-lib-version 2.0)
(set-info :category "crafted")
(set-info :status unsat)
(declare-fun a () (_ BitVec 64))
(declare-fun dummy () (_ BitVec 16))
(declare-fun v1 () (_ BitVec 64))
(declare-fun v2 () (_ BitVec 64))
(declare-fun v3 () (_ BitVec 64))
(declare-fun v4 () (_ BitVec 64))
(declare-fun v5 () (_ BitVec 64))
(declare-fun v6 () (_ BitVec 64))
(declare-fun v7 () (_ BitVec 64))
(declare-fun v8 () (_ BitVec 64))
(declare-fun v9 () (_ BitVec 64))
(declare-fun v10 () (_ BitVec 64))
(declare-fun v11 () (_ BitVec 64))
(declare-fun v12 () (_ BitVec 64))
(declare-fun v13 () (_ BitVec 64))
(declare-fun v14 () (_ BitVec 64))
(declare-fun v15 () (_ BitVec 64))
(declare-fun v16 () (_ BitVec 64))
(assert (let ((?v_0 ((_ extract 63 16) a)) (?v_1 ((_ extract 47 0) a))) (and (not (= ((_ extract 47 32) v1) ((_ extract 31 16) v1))) (not (= ((_ extract 47 32) v2) ((_ extract 31 16) v2))) (not (= ((_ extract 47 32) v3) ((_ extract 31 16) v3))) (not (= ((_ extract 47 32) v4) ((_ extract 31 16) v4))) (not (= ((_ extract 47 32) v5) ((_ extract 31 16) v5))) (not (= ((_ extract 47 32) v6) ((_ extract 31 16) v6))) (not (= ((_ extract 47 32) v7) ((_ extract 31 16) v7))) (not (= ((_ extract 47 32) v8) ((_ extract 31 16) v8))) (not (= ((_ extract 47 32) v9) ((_ extract 31 16) v9))) (not (= ((_ extract 47 32) v10) ((_ extract 31 16) v10))) (not (= ((_ extract 47 32) v11) ((_ extract 31 16) v11))) (not (= ((_ extract 47 32) v12) ((_ extract 31 16) v12))) (not (= ((_ extract 47 32) v13) ((_ extract 31 16) v13))) (not (= ((_ extract 47 32) v14) ((_ extract 31 16) v14))) (not (= ((_ extract 47 32) v15) ((_ extract 31 16) v15))) (not (= ((_ extract 47 32) v16) ((_ extract 31 16) v16))) (or (and (= ?v_0 (concat ((_ extract 63 32) v1) dummy)) (= ?v_1 (concat dummy ((_ extract 31 0) v1)))) (and (= ?v_0 (concat ((_ extract 63 32) v2) dummy)) (= ?v_1 (concat dummy ((_ extract 31 0) v2)))) (and (= ?v_0 (concat ((_ extract 63 32) v3) dummy)) (= ?v_1 (concat dummy ((_ extract 31 0) v3)))) (and (= ?v_0 (concat ((_ extract 63 32) v4) dummy)) (= ?v_1 (concat dummy ((_ extract 31 0) v4)))) (and (= ?v_0 (concat ((_ extract 63 32) v5) dummy)) (= ?v_1 (concat dummy ((_ extract 31 0) v5)))) (and (= ?v_0 (concat ((_ extract 63 32) v6) dummy)) (= ?v_1 (concat dummy ((_ extract 31 0) v6)))) (and (= ?v_0 (concat ((_ extract 63 32) v7) dummy)) (= ?v_1 (concat dummy ((_ extract 31 0) v7)))) (and (= ?v_0 (concat ((_ extract 63 32) v8) dummy)) (= ?v_1 (concat dummy ((_ extract 31 0) v8)))) (and (= ?v_0 (concat ((_ extract 63 32) v9) dummy)) (= ?v_1 (concat dummy ((_ extract 31 0) v9)))) (and (= ?v_0 (concat ((_ extract 63 32) v10) dummy)) (= ?v_1 (concat dummy ((_ extract 31 0) v10)))) (and (= ?v_0 (concat ((_ extract 63 32) v11) dummy)) (= ?v_1 (concat dummy ((_ extract 31 0) v11)))) (and (= ?v_0 (concat ((_ extract 63 32) v12) dummy)) (= ?v_1 (concat dummy ((_ extract 31 0) v12)))) (and (= ?v_0 (concat ((_ extract 63 32) v13) dummy)) (= ?v_1 (concat dummy ((_ extract 31 0) v13)))) (and (= ?v_0 (concat ((_ extract 63 32) v14) dummy)) (= ?v_1 (concat dummy ((_ extract 31 0) v14)))) (and (= ?v_0 (concat ((_ extract 63 32) v15) dummy)) (= ?v_1 (concat dummy ((_ extract 31 0) v15)))) (and (= ?v_0 (concat ((_ extract 63 32) v16) dummy)) (= ?v_1 (concat dummy ((_ extract 31 0) v16))))))))
(check-sat)
(exit)
