(load "2_46_exercise")
(load "2_47_exercise")
(load "2_48_exercise")

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))


(define (outline-painter)
  (lambda (frame)
  	(segment-painter (list (make-segment (origin-frame frame) (edge1-frame frame))
			       (make-segment (edge1-frame frame)  (v-frame frame))
			       (make-segment (edge2-frame frame)  (v-frame frame))
			       (make-segment (origin-frame frame) (edge2-frame frame))))))

(define (x-painter)
  (lambda (frame)
    	(segment-painter (list (make-segment (origin-frame frame) (v-frame frame))
			       (make-segment (edge1-frame frame) (edge2-frame frame))))))

(define (diamond-painter)
  (lambda (frame)
    	(segment-painter (list (make-segment (mid-vect (origin-frame frame) (edge1-frame frame)) 
					     (mid-vect (edge1-frame frame) (v-frame frame)))
			       (make-segment (mid-vect (edge1-frame frame) (v-frame frame))
					     (mid-vect (v-frame frame) (edge2-frame frame)))
			       (make-segment (mid-vect (v-frame frame) (edge2-frame frame))
					     (mid-vect (edge2-frame frame) (origin-frame frame)))
			       (make-segment (mid-vect (edge2-frame frame) (origin-frame frame))
					     (mid-vect (origin-frame frame) (edge1-frame frame)))))))

