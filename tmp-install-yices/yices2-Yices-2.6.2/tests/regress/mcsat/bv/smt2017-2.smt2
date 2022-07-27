(set-info :smt-lib-version 2.6)
(set-logic QF_BV)
(set-info :source | Stéphane Graham-Lengrand, Basic example #2 from SMT'2017 preliminary repoprt |)
(set-info :category "crafted")
;; (set-info :status sat)
(declare-fun y () (_ BitVec 12))
(declare-fun x2 () (_ BitVec 12))
(declare-fun x1 () (_ BitVec 12))
(assert (= (concat ((_ extract 2 0) x1) ((_ extract 7 5) x2))
           (concat ((_ extract 2 0) y) ((_ extract 7 5) y)))) ;; range r1 = 2:0, range r2 = 7:5
(assert (= (concat ((_ extract 4 3) y) ((_ extract 9 8) y))
           (concat ((_ extract 11 10) y) ((_ extract 11 10) y)))) ;; range r3 = 4:3, range r4 = 9:8, range r5 = 11:10
(assert (not (= ((_ extract 4 0) y) ((_ extract 9 5) y))))
(assert (= ((_ extract 4 0) x1) ((_ extract 9 5) x1))) ;; from now on, ad hoc stuff to force mcsat to start with x1 =* #b0...0 and x2 =* #b0...0
(assert (= ((_ extract 4 0) x2) ((_ extract 9 5) x2)))
(assert (not (= ((_ extract 5 0) x1) #b000001)))
(assert (not (= ((_ extract 5 0) x2) #b000001)))
(assert (not (= ((_ extract 10 6) x1) #b00010)))
(assert (not (= ((_ extract 10 6) x2) #b00010)))
(check-sat)
(exit)
