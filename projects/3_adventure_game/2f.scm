(load "load")
(load "adv-world")

(define (name obj) (ask obj 'name))
(define (inventory obj)
 (if (person? obj)
     (map name (ask obj 'possessions))
     (map name (ask obj 'things))))

(define (whereis person)
  (let ((where (ask person 'place)))
      (ask where 'name)))

(define (owner thing)
  (let ((possessor (ask thing 'possessor)))
    (if (person? possessor)
      	(ask possessor 'name)
	possessor)))
