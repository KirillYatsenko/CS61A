(define existing-value -1)

(define (f value)
  (cond ((eq? existing-value -1)
	   (begin
	     (set! existing-value value)
	     existing-value))
	(else 0)))
