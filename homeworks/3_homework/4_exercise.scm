;###################################################
;     Abelson & Sussman, exercise 1.16
;###################################################


(define (iter-fast-exp b n a)

    (define (even? number)
        (if (= 0 (modulo number 2))
            #t
            #f))

    (define (square number)
        (* number number))

  	(cond ((= n 0) a)
	      ((not (even? n)) 
	       		(iter-fast-exp b (- n 1) (* a b)))
	      ((even? n)
	       		(iter-fast-exp b (- n 2) (* a (square b))))))
	      