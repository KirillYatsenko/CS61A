; scheme-number package
(define (=zero-scheme-number? x) (= x 0))
; ...
(put '=zero? '(scheme-number scheme-number) =zero-scheme-number?)

; rational package
(define (=zero-rat? x) (and (= (numer x) 0) (= (denom x) 0)))
; ...
(put '=zero? '(rational rational) =zero-rat?)

; complex package
(define (=zero-complex? x) (and (= (imag-part x) 0) (= (real-part x) 0)))
; ...
(put '=zero? '(complex complex) =zero-complex?)

; make global
(define (=zero? x y) (apply-generic '=zero? x y))