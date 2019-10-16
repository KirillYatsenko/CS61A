(define (apply-substitution current old-value new-value)
  (if (equal? current old-value)
      new-value
      current))

(define (substitute items old-value new-value)
  (cond ((word? items) (apply-substitution items old-value new-value))
	((empty? items) nil)
     (else (cons (substitute (car items) old-value new-value)
	         (substitute (cdr items) old-value new-value)))))

