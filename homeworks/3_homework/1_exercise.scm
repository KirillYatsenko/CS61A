(define (squares numbers)
  (define (iter numbers squares)
	(if (empty? numbers) 
		squares
		(iter (butfirst numbers) (se squares (* (first numbers) (first numbers))))))

   (iter numbers '()))

