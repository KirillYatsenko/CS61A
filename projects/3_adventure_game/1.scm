(load "load")
(load "adv-world")


(define Dormitory (instantiate place 'Dormitory))
(define Me (instantiate person 'Me Dormitory))

(can-go Dormitory 'north 61A-Lab)

(define Kirin (instantiate place 'Kirin))

(can-go Soda 'north Kirin)
(can-go Kirin 'south Soda)

(define Potstickers (instantiate thing 'Potstickers))

(ask Kirin 'appear Potstickers)
