(load "load")
(load "adv-world")


(define Me (instantiate person 'Me))

(define Dormitory (instantiate place 'Dormitory))

(ask Dormitory 'appear Me)

(can-go Dormitory 'north 61A-Lab)

(define Kirin (instantiate place 'Kirin))

(can-go Soda 'north Kirin)

(define Potstickers (instantiate thing 'Potstickers))

(ask Kirin 'appear Potstickers)

