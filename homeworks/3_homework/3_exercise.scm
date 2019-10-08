(define (next-perfect number)
	(define (get-factors number current factors)
		(cond ((= number current) factors)
		      ((equal? 0 (modulo number current)) 
			 (get-factors number (+ 1 current) (se current factors)))
		      (else (get-factors number (+ 1 current) factors))))

	(define (sum numbers product)
		  (cond ((empty? numbers) product)
			(else (sum (butfirst numbers) (+ product (first numbers))))))

  	(if (= (sum (get-factors number 1 '()) 0) number)
	    number
	 (next-perfect (+ 1 number))))
