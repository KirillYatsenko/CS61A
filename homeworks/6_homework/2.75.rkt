#lang simply-scheme

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) x)
          ((eq? op 'angle) y)
          ((eq? op 'real-part)
           (* (magnitude x) (cos (angle y))))
          ((eq? op 'imag-part)
           (* (magnitude x) (sin (angle y))))
          (else
           (error "Unknown op: 
            MAKE-FROM-REAL-IMAG" op))))

  dispatch)