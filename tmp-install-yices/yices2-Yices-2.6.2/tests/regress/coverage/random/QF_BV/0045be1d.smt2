(set-info :source |fuzzsmt|)
(set-info :smt-lib-version 2.0)
(set-info :category "random")
(set-info :status unknown)
(set-logic QF_BV)
(declare-fun v0 () (_ BitVec 10))
(assert (let ((e1(_ bv4 3)))
(let ((e2 (bvneg v0)))
(let ((e3 (bvshl e2 v0)))
(let ((e4 (bvmul e2 v0)))
(let ((e5 (bvurem e2 e3)))
(let ((e6 (bvlshr e5 e4)))
(let ((e7 ((_ rotate_right 6) e5)))
(let ((e8 (bvxor e7 e2)))
(let ((e9 (bvor v0 e2)))
(let ((e10 (bvlshr v0 v0)))
(let ((e11 (bvsdiv ((_ sign_extend 7) e1) e3)))
(let ((e12 (distinct e9 e6)))
(let ((e13 (bvuge e2 e3)))
(let ((e14 (distinct e6 e8)))
(let ((e15 (bvuge e11 e10)))
(let ((e16 (bvule e7 v0)))
(let ((e17 (bvsle e2 v0)))
(let ((e18 (bvsgt v0 e2)))
(let ((e19 (= ((_ zero_extend 7) e1) e6)))
(let ((e20 (bvsgt v0 e3)))
(let ((e21 (bvsgt e8 e6)))
(let ((e22 (distinct e7 e8)))
(let ((e23 (bvsle e8 e5)))
(let ((e24 (bvult e2 e4)))
(let ((e25 (or e24 e17)))
(let ((e26 (=> e20 e16)))
(let ((e27 (not e13)))
(let ((e28 (and e15 e14)))
(let ((e29 (ite e12 e21 e27)))
(let ((e30 (= e26 e22)))
(let ((e31 (ite e30 e18 e25)))
(let ((e32 (= e19 e28)))
(let ((e33 (not e29)))
(let ((e34 (not e31)))
(let ((e35 (xor e34 e32)))
(let ((e36 (ite e23 e35 e33)))
(let ((e37 (and e36 (not (= e3 (_ bv0 10))))))
(let ((e38 (and e37 (not (= e3 (bvnot (_ bv0 10)))))))
e38
)))))))))))))))))))))))))))))))))))))))

(check-sat)
