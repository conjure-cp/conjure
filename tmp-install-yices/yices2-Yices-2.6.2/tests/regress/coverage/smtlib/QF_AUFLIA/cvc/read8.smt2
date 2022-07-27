(set-logic QF_AUFLIA)
(set-info :source |
Translated from old SVC processor verification benchmarks.  Contact Clark
Barrett at barrett@cs.nyu.edu for more information.

This benchmark was automatically translated into SMT-LIB format from
CVC format using CVC Lite
|)
(set-info :smt-lib-version 2.0)
(set-info :category "crafted")
(set-info :status unsat)
(declare-fun S () (Array Int Int))
(declare-fun S_prime () (Array Int Int))
(declare-fun a_prime () Int)
(declare-fun v_prime () Int)
(declare-fun aa () Int)
(declare-fun p () Bool)
(declare-fun v () Int)
(declare-fun vv () Int)
(declare-fun a () Int)
(declare-fun b () Int)
(declare-fun a1 () Int)
(declare-fun a2 () Int)
(declare-fun v2 () Int)
(declare-fun S2 () (Array Int Int))
(declare-fun v1 () Int)
(declare-fun S1 () (Array Int Int))
(declare-fun a3 () Int)
(declare-fun v3 () Int)
(declare-fun S3 () (Array Int Int))
(declare-fun a4 () Int)
(declare-fun v4 () Int)
(declare-fun S4 () (Array Int Int))
(declare-fun f (Int) Int)
(declare-fun z () Int)
(declare-fun x () Int)
(declare-fun g (Int) Int)
(declare-fun y () Int)
(assert (let ((?v_1 (select S1 a1)) (?v_2 (select S2 a2)) (?v_3 (select S3 a3)) (?v_5 (f z))) (let ((?v_6 (= ?v_5 a1)) (?v_4 (select S2 a1)) (?v_0 (store S4 a4 v4))) (not (ite (ite (ite (= S (store S_prime a_prime v_prime)) (ite (= (store S aa (ite p v vv)) (store S a v)) (ite (ite (= a a_prime) false true) (ite (= a aa) false true) false) false) false) (ite (= (select S_prime a) v) (ite (ite (= a_prime b) false true) (= (select S b) (select S_prime b)) true) false) true) (ite (ite (ite (ite (= (ite (= a1 a2) v2 ?v_4) v1) (= (store (store S2 a2 v2) a1 ?v_1) S1) false) (ite (ite (= (ite (= a2 a3) v3 (select S3 a2)) v2) (= (store (store S3 a3 v3) a2 ?v_2) S2) false) (ite (ite (= (ite (= a3 a4) v4 (select S4 a3)) v3) (= (store ?v_0 a3 ?v_3) S3) false) (ite (ite (= (ite (= a1 a4) v4 (select S4 a1)) v1) (= (store ?v_0 a1 ?v_1) S1) false) (ite (= ?v_1 v1) (ite (= ?v_2 v2) (ite (= ?v_3 v3) (= (select S4 a4) v4) false) false) false) false) false) false) false) (= (select S1 b) (select S3 b)) true) (ite (ite (ite (= (ite ?v_6 v2 ?v_4) (f x)) (= (store (store S2 ?v_5 v2) a1 ?v_1) S1) false) (= S1 (store S3 (g z) v3)) false) (ite (ite (= x y) (ite (= y z) (ite (ite ?v_6 false true) (ite (= (g x) ?v_5) false true) false) false) false) (= (select S3 ?v_5) v2) true) true) false) false)))))
(check-sat)
(exit)
