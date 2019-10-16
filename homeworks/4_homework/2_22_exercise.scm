; The issue is in the example we constructing our list from in to out, therefore
; E.g items = 2 4 7 => (cons(cons(cons 2) 4) 7) <=>   2<=4<= 7

;Solution using append
(define (square-list items)
 (define (iter things answer)
   (if (null? things)
	answer
	(iter (cdr things) (append answer (list (square (car things)))))))

 (iter items nil)) 
