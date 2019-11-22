#lang simply-scheme

(define (make-mobile left right)
  (cons left right))


(define (make-branch length structure)
  (cons length structure))

(define (branch-structure branch)
  (cdr branch))


(define (branch-left mobile)
  (car mobile))

(define (branch-right mobile)
  (cdr mobile))



(define (total-weight mobile)
    (if (number? mobile)
        mobile
        (+ (total-weight (branch-structure (branch-left mobile )))
           (total-weight (branch-structure (branch-right mobile))))))


(define small-mobile
  (make-mobile (make-branch 1 10) (make-branch 1 7)))


(trace total-weight)

(total-weight small-mobile)