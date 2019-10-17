(define (split primary-split secondary-split)
 (define (split-generic painter n)
  (if (= n 0)
      painter
      (let ((smaller (split-generic painter (- n 1))))
        (primary-split painter (secondary-split smaller smaller)))))
 
 split-generic)
