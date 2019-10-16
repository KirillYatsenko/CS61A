(load "2_10_exercise")

(define (make-center-percent center percentage)
	(let ((percent-value (/ (* center percentage) 100)))
	  	(make-interval (- center percent-value) 
			       (+ center percent-value))))
