(load "2_46_exercise")

(define (make-segment vec1 vec2)
  (cons vec1 vec2))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))
