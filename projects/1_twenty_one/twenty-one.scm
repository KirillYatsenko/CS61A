4(define (best-total cards)

	(define (find-max-total totals current-max)
		(cond ((empty? totals) current-max)
		      ( (and (> (first totals) current-max) (<= (first totals) 21))   (find-max-total (butfirst totals) (first totals)))	
		      (else (find-max-total (butfirst totals) current-max))))

	(define (all-total cards)
		(all-total-help cards 0 '()))

	(define (all-total-help cards total totals)
		
	  	(define (is-jocker card)
		   (cond ((equal? (first card) 'r) #t)
		   (else #f)))
		
		(define (is-ace card)
		   (cond ((equal? (first card) 'a) #t)
		   (else #f)))
		
		(define (test-sentence sente count)
			(cond ((= count 0) sente)
			      (else (test-sentence (se sente 'ab) (- count 1)))))
			
		(cond 
		      ((empty? cards) total )
		      ((not (or (is-ace (first cards)) (is-jocker (first cards))))
		      	(se totals (all-total-help (butfirst cards) (+ total (card-value (first cards))) totals )))    
		      ((is-ace (first cards)) 
								(se totals (all-total-help (butfirst cards) (+ total 1) totals) 
												   (all-total-help (butfirst cards) (+ total 11) totals)))
		      ((is-jocker (first cards)) 
							(se totals (all-total-help (butfirst cards) (+ total 1) totals) 
									(all-total-help (butfirst cards) (+ total 2) totals)
									(all-total-help (butfirst cards) (+ total 3) totals)
									(all-total-help (butfirst cards) (+ total 4) totals)
									(all-total-help (butfirst cards) (+ total 5) totals)
									(all-total-help (butfirst cards) (+ total 6) totals)
									(all-total-help (butfirst cards) (+ total 7) totals)s
									(all-total-help (butfirst cards) (+ total 8) totals)
									(all-total-help (butfirst cards) (+ total 9) totals)
									(all-total-help (butfirst cards) (+ total 10) totals)
									(all-total-help (butfirst cards) (+ total 11) totals)))))		   

	(find-max-total (all-total cards) (first (all-total cards)) ))


(define (card-value card)	
		(card-value-helper card (word)))

		(define (card-value-helper card value)
				(cond ((empty? card) value)
				      ((or(member? (first card) 'ra123456789) (equal? (first card) '0)) 
						(card-value-helper (butfirst card) (word value (first card))))
				      ((member? (first card) 'jqk) 10)    
				(else value)))



;#####################  Strategies  ###################################
(define (majority 1st-strategy 2nd-strategy 3rd-strategy)
	(lambda (customer dealer deck)
		(cond
		      ((and (1st-strategy customer dealer deck) (2nd-strategy customer dealer deck)) #t )
		      ((and (1st-strategy customer dealer deck) (3rd-strategy customer dealer deck)) #t )
		      (else  #f ))))

(define (stop-at n)
	(lambda (customer-hand-so-far dealer-up-card rest-of-deck) 
		(cond ((>= (best-total customer-hand-so-far) n) #f) (else #t))))

(define (suit-strategy suit include-strategy not-include-strategy)
	(lambda (customer-hand-so-far dealer-up-card rest-of-deck)
	  (cond ((exists-at-least-1 customer-hand-so-far (se (word 'a suit) (word '2 suit) (word '3 suit) (word '4 suit) (word '5 suit) (word '6 suit) (word '7 suit) (word '8 suit) (word '9 suit) (word '10 suit) (word 'j suit) (word 'q suit) (word 'k suit) )) include-strategy ) (else not-include-strategy) ) ))

(define (valentine customer-hand-so-far dealer-up-card rest-of-deck)
	(suit-strategy 'a (stop-at 19) (stop-at 17)))

(define (dealer-sensitive dealer-hand customer-hand)
	(cond ((and (exists-at-least-1 dealer-hand '(ac ad ah as 7c 7d 7h 7s 8c 8d 8h 8s 9c 9d 9h 9s 10c 10d 10h 10s jc jd jh js qc qd qh qs kc kd kh ks)) (< (best-total customer-hand) 17)) #t)
	      ((and (exists-at-least-1 dealer-hand '(2c 2d 2h 2s 3c 3d 3h 3s 4c 4d 4h 4s 5c 5d 5h 5s 6c 6d 6h 6s jc jd jh js qc qd qh qs kc kd kh ks)) (< (best-total customer-hand) 12)) #t)
	(else #f)))


;######################################################################


(define (exists-at-least-1 in-cards cards)
	(cond ((empty? cards) #f)
	      ((member? (first cards) in-cards ) #t)
	(else (exists-at-least-1 in-cards (butfirst cards)))))

(define (play-n strategy dealer-strategy n)
	(define (play-n-helper strategy dealer-strategy n count win loss )
		(let ((twenty-one-result (twenty-one strategy dealer-strategy)))
			(cond ((equal? count n) (- win loss) )
			      ((equal? twenty-one-result -1 )  (play-n-helper strategy dealer-strategy n (+ count 1) win (+ loss 1)))
			      ((equal? twenty-one-result 0  )  (play-n-helper strategy dealer-strategy n (+ count 1) win loss      ))
			      ((equal? twenty-one-result 1  )  (play-n-helper strategy dealer-strategy n (+ count 1) (+ win 1) loss)))))

	(play-n-helper strategy dealer-strategy n 0 0 0))
			
				
(define (twenty-one strategy dealer-strategy)
  (define (play-dealer customer-hand dealer-hand-so-far rest-of-deck)
    (cond ((> (best-total dealer-hand-so-far) 21) 1)
	  ((dealer-strategy dealer-hand-so-far customer-hand)
	   (play-dealer customer-hand
			(se dealer-hand-so-far (first rest-of-deck))
			(bf rest-of-deck)))
	  ((< (best-total customer-hand) (best-total dealer-hand-so-far)) -1)
	  ((= (best-total customer-hand) (best-total dealer-hand-so-far)) 0)
	  (else 1)))

  (define (play-customer customer-hand-so-far dealer-up-card rest-of-deck)
    (cond ((> (best-total customer-hand-so-far) 21) -1)
	  ((strategy customer-hand-so-far dealer-up-card rest-of-deck)
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
    (every (lambda (rank) (word rank s)) '(R A 2 3 4 5 6 7 8 9 10 J Q K)) )
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