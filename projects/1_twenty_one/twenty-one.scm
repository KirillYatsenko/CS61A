(define (best-total cards)

	(define (find-max-total totals current-max)
		(cond ((empty? totals) current-max)
		      ((> (first totals) current-max) (find-max-total (butfirst totals) (first totals)))	
		      (else (find-max-total (butfirst totals) current-max))))

	(define (all-total cards)
		(all-total-help cards 0 '()))

	(define (all-total-help cards total totals)

		(define (is-ace card)
		   (cond ((equal? (first card) 'a) #t)
		   (else #f)))

		(define (card-value card)	
			(card-value-helper card (word)))

		(define (card-value-helper card value)
				(cond ((empty? card) value)
				      ((or(member? (first card) '123456789) (equal? (first card) '0)) 
						(card-value-helper (butfirst card) (word value (first card))))	      
				(else value)))
		
		(define (test-sentence sente count)
			(cond ((= count 0) sente)
			      (else (test-sentence (se sente 'ab) (- count 1)))))
			
		(cond 
		      ((> total 21) 0 )
		      ((empty? cards) total )
		      ((not (is-ace (first cards)))
		      	(se totals (all-total-help (butfirst cards) (+ total (card-value (first cards))) totals )))    
		      ((is-ace (first cards)) 
			(se totals (all-total-help (butfirst cards) (+ total 1) totals) 
				   (all-total-help (butfirst cards) (+ total 11) totals)))))

	(find-max-total (all-total cards) 0))


(define (stop-at-17 hand)
	   (cond ((>= hand 17) #f)
	   (else #t)))

(define (twenty-one strategy)
  (define (play-dealer customer-hand dealer-hand-so-far rest-of-deck)
    (cond ((> (best-total dealer-hand-so-far) 21) 1)
	  ((< (best-total dealer-hand-so-far) 17)
	   (play-dealer customer-hand
			(se dealer-hand-so-far (first rest-of-deck))
			(bf rest-of-deck)))
	  ((< (best-total customer-hand) (best-total dealer-hand-so-far)) -1)
	  ((= (best-total customer-hand) (best-total dealer-hand-so-far)) 0)
	  (else 1)))

  (define (play-customer customer-hand-so-far dealer-up-card rest-of-deck)
    (cond ((> (best-total customer-hand-so-far) 21) -1)
	  ((strategy customer-hand-so-far dealer-up-card)
	   (play-customer (se customer-hand-so-far (first rest-of-deck))
			  dealer-up-card
			  (bf rest-of-deck)))
	  (else
	   (play-dealer customer-hand-so-far
			(se dealer-up-card (first rest-of-deck))
			(bf rest-of-deck)))))

  (let ((deck (make-deck)))
    (play-customer (se (first deck) (first (bf deck)))
		   (first (bf (bf deck)))
		   (bf (bf (bf deck))))) )

(define (make-ordered-deck)
  (define (make-suit s)
    (every (lambda (rank) (word rank s)) '(A 2 3 4 5 6 7 8 9 10 J Q K)) )
  (se (make-suit 'H) (make-suit 'S) (make-suit 'D) (make-suit 'C)) )

(define (make-deck)
  (define (shuffle deck size)
    (define (move-card in out which)
      (if (= which 0)
	  (se (first in) (shuffle (se (bf in) out) (- size 1)))
	  (move-card (bf in) (se (first in) out) (- which 1)) ))
    (if (= size 0)
	deck
    	(move-card deck '() (random size)) ))
  (shuffle (make-ordered-deck) 52) )