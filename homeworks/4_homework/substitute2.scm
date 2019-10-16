(define (find-index elem items)
  (define (find-index-iter elem items count)
	  (cond ((empty? items) -1)
		((equal? elem (first items)) count)
		(else (find-index-iter elem (butfirst items) (+ 1 count)))))
 
  (find-index-iter elem items 0))


(define (get-by-index index items)
  (define (get-by-index-iter index items current-index)
    (if (= index current-index)
	(car items)
	(get-by-index-iter index (cdr items) (+ 1 current-index))))

  (get-by-index-iter index items 0))


(define (apply-substitution current old-values new-values)
  (if (member? current old-values)
      (get-by-index (find-index current old-values) new-values)
      current))

(define (substitute2 items old-values new-values)
  (cond ((word? items) (apply-substitution items old-values new-values))
	((empty? items) nil)
     (else (cons (substitute2 (car items) old-values new-values)
	         (substitute2 (cdr items) old-values new-values)))))
