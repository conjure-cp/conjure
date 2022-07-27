(set-logic QF_BV)
(set-info :source |
We verify the correctness of an unsigned multiplication
overflow detection unit, which is described in
"Combined Unsigned and Two's Complement Saturating Multipliers"
by M. Schulte et al.

Bit-width: 48

Contributed by Robert Brummayer (robert.brummayer@gmail.com).
|)
(set-info :smt-lib-version 2.0)
(set-info :category "industrial")
(set-info :status unsat)
(declare-fun v1 () (_ BitVec 48))
(declare-fun v2 () (_ BitVec 48))
(assert (let ((?v_0 ((_ extract 47 47) v2))) (let ((?v_1 (bvand (bvnot ?v_0) (bvnot ((_ extract 46 46) v2))))) (let ((?v_2 (bvand ?v_1 (bvnot ((_ extract 45 45) v2))))) (let ((?v_3 (bvand ?v_2 (bvnot ((_ extract 44 44) v2))))) (let ((?v_4 (bvand ?v_3 (bvnot ((_ extract 43 43) v2))))) (let ((?v_5 (bvand ?v_4 (bvnot ((_ extract 42 42) v2))))) (let ((?v_6 (bvand ?v_5 (bvnot ((_ extract 41 41) v2))))) (let ((?v_7 (bvand ?v_6 (bvnot ((_ extract 40 40) v2))))) (let ((?v_8 (bvand ?v_7 (bvnot ((_ extract 39 39) v2))))) (let ((?v_9 (bvand ?v_8 (bvnot ((_ extract 38 38) v2))))) (let ((?v_10 (bvand ?v_9 (bvnot ((_ extract 37 37) v2))))) (let ((?v_11 (bvand ?v_10 (bvnot ((_ extract 36 36) v2))))) (let ((?v_12 (bvand ?v_11 (bvnot ((_ extract 35 35) v2))))) (let ((?v_13 (bvand ?v_12 (bvnot ((_ extract 34 34) v2))))) (let ((?v_14 (bvand ?v_13 (bvnot ((_ extract 33 33) v2))))) (let ((?v_15 (bvand ?v_14 (bvnot ((_ extract 32 32) v2))))) (let ((?v_16 (bvand ?v_15 (bvnot ((_ extract 31 31) v2))))) (let ((?v_17 (bvand ?v_16 (bvnot ((_ extract 30 30) v2))))) (let ((?v_18 (bvand ?v_17 (bvnot ((_ extract 29 29) v2))))) (let ((?v_19 (bvand ?v_18 (bvnot ((_ extract 28 28) v2))))) (let ((?v_20 (bvand ?v_19 (bvnot ((_ extract 27 27) v2))))) (let ((?v_21 (bvand ?v_20 (bvnot ((_ extract 26 26) v2))))) (let ((?v_22 (bvand ?v_21 (bvnot ((_ extract 25 25) v2))))) (let ((?v_23 (bvand ?v_22 (bvnot ((_ extract 24 24) v2))))) (let ((?v_24 (bvand ?v_23 (bvnot ((_ extract 23 23) v2))))) (let ((?v_25 (bvand ?v_24 (bvnot ((_ extract 22 22) v2))))) (let ((?v_26 (bvand ?v_25 (bvnot ((_ extract 21 21) v2))))) (let ((?v_27 (bvand ?v_26 (bvnot ((_ extract 20 20) v2))))) (let ((?v_28 (bvand ?v_27 (bvnot ((_ extract 19 19) v2))))) (let ((?v_29 (bvand ?v_28 (bvnot ((_ extract 18 18) v2))))) (let ((?v_30 (bvand ?v_29 (bvnot ((_ extract 17 17) v2))))) (let ((?v_31 (bvand ?v_30 (bvnot ((_ extract 16 16) v2))))) (let ((?v_32 (bvand ?v_31 (bvnot ((_ extract 15 15) v2))))) (let ((?v_33 (bvand ?v_32 (bvnot ((_ extract 14 14) v2))))) (let ((?v_34 (bvand ?v_33 (bvnot ((_ extract 13 13) v2))))) (let ((?v_35 (bvand ?v_34 (bvnot ((_ extract 12 12) v2))))) (let ((?v_36 (bvand ?v_35 (bvnot ((_ extract 11 11) v2))))) (let ((?v_37 (bvand ?v_36 (bvnot ((_ extract 10 10) v2))))) (let ((?v_38 (bvand ?v_37 (bvnot ((_ extract 9 9) v2))))) (let ((?v_39 (bvand ?v_38 (bvnot ((_ extract 8 8) v2))))) (let ((?v_40 (bvand ?v_39 (bvnot ((_ extract 7 7) v2))))) (let ((?v_41 (bvand ?v_40 (bvnot ((_ extract 6 6) v2))))) (let ((?v_42 (bvand ?v_41 (bvnot ((_ extract 5 5) v2))))) (let ((?v_43 (bvand ?v_42 (bvnot ((_ extract 4 4) v2))))) (let ((?v_44 (bvand ?v_43 (bvnot ((_ extract 3 3) v2))))) (let ((?v_45 (bvand ?v_44 (bvnot ((_ extract 2 2) v2))))) (not (= (bvnot (ite (= (bvnot (ite (= ((_ extract 95 48) (bvmul (concat (_ bv0 48) v1) (concat (_ bv0 48) v2))) (_ bv0 48)) (_ bv1 1) (_ bv0 1))) (bvnot (bvand (bvand (bvand (bvand (bvand (bvand (bvand (bvand (bvand (bvand (bvand (bvand (bvand (bvand (bvand (bvand (bvand (bvand (bvand (bvand (bvand (bvand (bvand (bvand (bvand (bvand (bvand (bvand (bvand (bvand (bvand (bvand (bvand (bvand (bvand (bvand (bvand (bvand (bvand (bvand (bvand (bvand (bvand (bvand (bvand (bvand (bvand (bvnot (bvand ((_ extract 1 1) v1) ?v_0)) (bvnot (bvand ((_ extract 2 2) v1) (bvnot ?v_1)))) (bvnot (bvand ((_ extract 3 3) v1) (bvnot ?v_2)))) (bvnot (bvand ((_ extract 4 4) v1) (bvnot ?v_3)))) (bvnot (bvand ((_ extract 5 5) v1) (bvnot ?v_4)))) (bvnot (bvand ((_ extract 6 6) v1) (bvnot ?v_5)))) (bvnot (bvand ((_ extract 7 7) v1) (bvnot ?v_6)))) (bvnot (bvand ((_ extract 8 8) v1) (bvnot ?v_7)))) (bvnot (bvand ((_ extract 9 9) v1) (bvnot ?v_8)))) (bvnot (bvand ((_ extract 10 10) v1) (bvnot ?v_9)))) (bvnot (bvand ((_ extract 11 11) v1) (bvnot ?v_10)))) (bvnot (bvand ((_ extract 12 12) v1) (bvnot ?v_11)))) (bvnot (bvand ((_ extract 13 13) v1) (bvnot ?v_12)))) (bvnot (bvand ((_ extract 14 14) v1) (bvnot ?v_13)))) (bvnot (bvand ((_ extract 15 15) v1) (bvnot ?v_14)))) (bvnot (bvand ((_ extract 16 16) v1) (bvnot ?v_15)))) (bvnot (bvand ((_ extract 17 17) v1) (bvnot ?v_16)))) (bvnot (bvand ((_ extract 18 18) v1) (bvnot ?v_17)))) (bvnot (bvand ((_ extract 19 19) v1) (bvnot ?v_18)))) (bvnot (bvand ((_ extract 20 20) v1) (bvnot ?v_19)))) (bvnot (bvand ((_ extract 21 21) v1) (bvnot ?v_20)))) (bvnot (bvand ((_ extract 22 22) v1) (bvnot ?v_21)))) (bvnot (bvand ((_ extract 23 23) v1) (bvnot ?v_22)))) (bvnot (bvand ((_ extract 24 24) v1) (bvnot ?v_23)))) (bvnot (bvand ((_ extract 25 25) v1) (bvnot ?v_24)))) (bvnot (bvand ((_ extract 26 26) v1) (bvnot ?v_25)))) (bvnot (bvand ((_ extract 27 27) v1) (bvnot ?v_26)))) (bvnot (bvand ((_ extract 28 28) v1) (bvnot ?v_27)))) (bvnot (bvand ((_ extract 29 29) v1) (bvnot ?v_28)))) (bvnot (bvand ((_ extract 30 30) v1) (bvnot ?v_29)))) (bvnot (bvand ((_ extract 31 31) v1) (bvnot ?v_30)))) (bvnot (bvand ((_ extract 32 32) v1) (bvnot ?v_31)))) (bvnot (bvand ((_ extract 33 33) v1) (bvnot ?v_32)))) (bvnot (bvand ((_ extract 34 34) v1) (bvnot ?v_33)))) (bvnot (bvand ((_ extract 35 35) v1) (bvnot ?v_34)))) (bvnot (bvand ((_ extract 36 36) v1) (bvnot ?v_35)))) (bvnot (bvand ((_ extract 37 37) v1) (bvnot ?v_36)))) (bvnot (bvand ((_ extract 38 38) v1) (bvnot ?v_37)))) (bvnot (bvand ((_ extract 39 39) v1) (bvnot ?v_38)))) (bvnot (bvand ((_ extract 40 40) v1) (bvnot ?v_39)))) (bvnot (bvand ((_ extract 41 41) v1) (bvnot ?v_40)))) (bvnot (bvand ((_ extract 42 42) v1) (bvnot ?v_41)))) (bvnot (bvand ((_ extract 43 43) v1) (bvnot ?v_42)))) (bvnot (bvand ((_ extract 44 44) v1) (bvnot ?v_43)))) (bvnot (bvand ((_ extract 45 45) v1) (bvnot ?v_44)))) (bvnot (bvand ((_ extract 46 46) v1) (bvnot ?v_45)))) (bvnot (bvand ((_ extract 47 47) v1) (bvnot (bvand ?v_45 (bvnot ((_ extract 1 1) v2))))))) (bvnot ((_ extract 48 48) (bvmul (concat (_ bv0 1) v1) (concat (_ bv0 1) v2))))))) (_ bv1 1) (_ bv0 1))) (_ bv0 1))))))))))))))))))))))))))))))))))))))))))))))))))
(check-sat)
(exit)
