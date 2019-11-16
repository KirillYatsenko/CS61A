(define (type? items predicate)
	(cond ((empty? items) #t)
	      ((not (predicate (first items))) #f)
	      (else (type? (butfirst items) predicate))))

(define (numbers? items)
  (type? items (lambda (x) (number? x))))

(define (symbols? items)
  (type? items (lambda (x) (symbol? x))))

(define (lists? items)
  (type? items (lambda (x) (list? x))))

(define (empty-lists? items)
  (type? items (lambda (x) (empty? x))))

(define (list-different-size? items)
  (not (eq? (empty-lists? (list (first items)))
          (empty-lists? (list (butfirst items))))))


(define (equal? el1 el2)
  (cond
    ((empty-lists?             (list el1 el2)) #t)
	((list-different-size?     (list el1 el2)) #f)
    ((numbers?                 (list el1 el2)) (=   el1 el2))
	((symbols?                 (list el1 el2)) (eq? el1 el2))
	((lists?                   (list el1 el2)) 
	 (and (equal? (car el1) (car el2))
	      (equal? (cdr el1) (cdr el2))))))