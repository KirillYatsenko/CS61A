(define (make-vect x y)
    (cons x y))

(define (xcor-vect vec)
    (car vec))

(define (ycor-vect vec)
    (cdr vec))


(define (add-vect vec1 vec2)
    (make-vect (+ (xcor-vect vec1) (xcor-vect vec2)) 
               (+ (ycor-vect vec1) (ycor-vect vec2))))

(define (sub-vect vec1 vec2)
    (make-vect (- (xcor-vect vec1) (xcor-vect vec2)) 
               (- (ycor-vect vec1) (ycor-vect vec2))))

(define (scale-vect vec scalar)
    (make-vect (* scalar (xcor-vect vec) (* scalar (ycor-vect vec)))))