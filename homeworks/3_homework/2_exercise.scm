(define (multiply numbers)
	(define (iter numbers product)
	  (cond ((empty? numbers) product)
		(else (iter (butfirst numbers) (* product (first numbers))))))
         
	(iter (butfirst numbers)  (first numbers)))
