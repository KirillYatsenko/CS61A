(load "interval-primitives")
(load "2_8_exercise")

(define (spans-zero? x)
  	(if (and (<= (lower-bound x) 0) (>= (upper-bound x) 0))
	    #t
	    #f))

(define (div-interval x y)
  (if (spans-zero? y)
      	  '(Divide By Zero Interval Not Allowed)
	  (mul-interval x
			(make-interval (/ 1.0 (upper-bound y))
				       (/ 1.0 (lower-bound y))))))
