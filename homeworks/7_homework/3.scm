(define (shuffle deck)
		(if (null? deck) ’()
	     	 (let ((card (nth (random (length deck)) deck)))
			(cons card (shuffle (remove card deck))))))

(define ordered-deck ’(AH 2H 3H ... QH KH AS 2S ... QC KC))

(define-class (deck))
	      (instance-vars (deck (shuffle ordered-deck)))
	      (instance-vars (temp))
	      (method (deal)
		      (cond 
			((empty? cards) '())
			(else ((set! temp (first deck)
			       (set! deck (butfirst deck))
			       temp)))))
	      (method (empty?)
		      (empty? deck)))

	    	      
