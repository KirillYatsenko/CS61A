(define-class (random-generator range)
	(instance-vars (count 0))
	(method (number)
	   (set! count (+ count 1))
	   (random (range))))


	
