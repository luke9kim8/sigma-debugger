(set-logic QF_NIA)
(declare-const x Bool)
(declare-fun U () Int)
(declare-fun Z () Int)
(assert (or x (and (> Z 0) (= 0 (+ U Z Z)) (> (+ U (* Z Z)) 0))))
(check-sat)
