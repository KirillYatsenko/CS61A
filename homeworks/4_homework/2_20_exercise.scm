(load "2_17_exercise")

(define (even? number)
  (if (equal? 0 (modulo number 2))
       #t
       #f))

(define (odd? number)
  (not (even? number)))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (same-parity . numbers)
  (if (even? (list-ref numbers 0)) 
      (filter even? numbers)
      (filter odd? numbers)))
