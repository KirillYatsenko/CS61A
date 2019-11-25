; scheme-number package
(define (raise-to-rational x) (make-rat x 0))
(put 'raise '(scheme-number scheme-number) raise-to-rational?)

; rational package
(define (raise-to-real x) (make-real (/ (numer n) (denom n))))
(put 'raise '(rational rational) raise-to-real)

; real package
(define (raise-to-complex x) (make-complex-from-real-imag x 0)
(put 'raise '(rational rational) raise-to-complex)


; make global
(define (raise x) (apply-generic 'raise x))