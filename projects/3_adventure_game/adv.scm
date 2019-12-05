;; ADV.SCM
;; This file contains the definitions for the objects in the adventure
;; game and some utility procedures.

(define-class (basic-object)
  (instance-vars
    (properties (make-table)))
  (method (put property value)
	  (insert! property value properties))
  (default-method
           (lookup message properties)))


(define-class (place name)
  (parent (basic-object))
  (instance-vars
   (directions-and-neighbors '())
   (things '())
   (people '())
   (entry-procs '())
   (exit-procs '()))
  (method (place?) #t)
  (method (peoples-possesions)
	  (map (lambda (person)
		 (ask person 'possessions)) 
	       people))
  (method (things-with-no-owner)
	  (filter (lambda (thing)
	            (let ((possessed-things people-possessions)
			   (memq thing possessed-things))))
		  things))
  (method (may-enter? person) #t)
  (method (type) 'place)
  (method (neighbors) (map cdr directions-and-neighbors))
  (method (exits) (map car directions-and-neighbors))
  (method (look-in direction)
    (let ((pair (assoc direction directions-and-neighbors)))
      (if (not pair)
	  '()                     ;; nothing in that direction
	  (cdr pair))))           ;; return the place object
  (method (appear new-thing)
    (if (memq new-thing things)
	(error "Thing already in this place" (list name new-thing)))
    (set! things (cons new-thing things))
    'appeared)
  (method (enter new-person)
    (if (memq new-person people)
	(error "Person already in this place" (list name new-person)))
    (for-each (lambda (person) (ask person 'notice new-person)) people) 
    (set! people (cons new-person people))
    (for-each (lambda (proc) (proc)) entry-procs)
    'appeared)
  (method (gone thing)
    (if (not (memq thing things))
	(error "Disappearing thing not here" (list name thing)))
    (set! things (delete thing things)) 
    'disappeared)
  (method (exit person)
    (for-each (lambda (proc) (proc)) exit-procs)
    (if (not (memq person people))
	(error "Disappearing person not here" (list name person)))
    (set! people (delete person people)) 
    'disappeared)

  (method (new-neighbor direction neighbor)
    (if (assoc direction directions-and-neighbors)
	(error "Direction already assigned a neighbor" (list name direction)))
    (set! directions-and-neighbors
	  (cons (cons direction neighbor) directions-and-neighbors))
    'connected)

  (method (add-entry-procedure proc)
    (set! entry-procs (cons proc entry-procs)))
  (method (add-exit-procedure proc)
    (set! exit-procs (cons proc exit-procs)))
  (method (remove-entry-procedure proc)
    (set! entry-procs (delete proc entry-procs)))
  (method (remove-exit-procedure proc)
    (set! exit-procs (delete proc exit-procs)))
  (method (clear-all-procs)
    (set! exit-procs '())
    (set! entry-procs '())
    'cleared) )

(define-class (hotspot password)
   (instace-vars
     (connected-laptops '()))
   (initialize 
     ((add-entry-procedure (lambda () (print "provide password")))))
   (method (hotspot?) #t)
   (method (gone thing)
	   (if (laptop? thing)
	     	(set! connected-laptops (delete thing connected-laptops)))
	   (ask super 'gone thing))
   (method (connect laptop provided-password)
	(cond ((not (memq laptop (things)))
	         (error "laptop not at hotstop"))
	      ((not (eq? password provided-password))
	       	 (error "passoword not correct"))
	      (else 
                ((set! connected-laptops (cons laptop connected-laptops))))))
   (method (surf laptop url)
	 (if (not (memq laptop connected-laptops))
	        (error "connect first!")
	        (system (string-append "lynx" url)))))

(define-class (locked-place name)
  (parent (place name))
  (instance-vars
    (is-locked #t))
  (method (may-enter? person)
    (not is-locked))
  (method (unlock)
    (set! is-locked #f)))

(define-class (garage name)
  (parent (place name))
  (class-vars (count 0))
  (instance-vars
    (ticket-cars-dictionary (make-table)))
  (method (generate-ticket)
     (set! count(+ count 1))
     (let ((ticket (instantiate ticket count)))
       (insert! count ticket ticket-cars-dictionary)
       ticket))
  (method (park person-car)
    (let ((owner (ask person-car 'possessor)))
     (enter owner)
     (let ((new-ticket (generate-ticket)))
      (ask owner 'lose person-car)
      (ask owner 'take new-ticket))))
  (method (unpark ticket)
	  (if (not (eq? (ask ticket 'name) 'ticket))
	      (error "formal is not a ticket")
	      (let ((owner-car (lookup (ask ticket 'number) ticket-cars-dictionary))))
	     	 (if (not owner-car)
		     (error "no car with given ticket number")
		     (let ((ticket-owner (ask ticket 'possessor)))
		          (ask ticket-owner 'lose ticket)
		          (ask ticket-owner 'take owner-car)
			  (insert! (ask ticket 'number) #f ticket-cars-dictionary))))))



(define-class (person name place)
  (parent (basic-object))
  (instance-vars
   (possessions '())
   (saying ""))
  (initialize
   (ask place 'enter self)
   (ask self 'put 'strength 100))
  (method (person?) #t)
  (method (type) 'person)
  (method (take-all)
	 (let ((unowned-things (ask place 'things-with-no-owner)))
	   (for-each
	     (lambda (thing)
	       (take thing)))))
  
  (method (look-around)
    (map (lambda (obj) (ask obj 'name))
	 (filter (lambda (thing) (not (eq? thing self)))
		 (append (ask place 'things) (ask place 'people)))))
  (method (take thing)
    (cond ((not (thing? thing)) (error "Not a thing" thing))
	  ((not (memq thing (ask place 'things)))
	   (error "Thing taken not at this place"
		  (list (ask place 'name) thing)))
	  ((memq thing possessions) (error "You already have it!"))
	  (else
	   (announce-take name thing)
	   (set! possessions (cons thing possessions))
	       
	   ;; If somebody already has this object...
	   (for-each
	    (lambda (pers)
	      (if (and (not (eq? pers self)) ; ignore myself
		       (memq thing (ask pers 'possessions)))
		  (begin
		    (if (< (ask self 'strength) (ask pers 'strength))
		        (error "You are weak!")
		        ((ask pers 'lose thing)
		         (have-fit pers))))))
	    (ask place 'people))
	       
	   (ask thing 'change-possessor self)
	   'taken)))

  (method (lose thing)
    (set! possessions (delete thing possessions))
    (ask thing 'change-possessor 'no-one)
    'lost)
  (method (talk) (print saying))
  (method (set-talk string) (set! saying string))
  (method (exits) (ask place 'exits))
  (method (notice person) (ask self 'talk))
  (method (go direction)
    (let ((new-place (ask place 'look-in direction)))
      (cond ((null? new-place)
	     (error "Can't go" direction))
	    ((not (ask new-place 'may-enter? self))
	     (error "Place is locked"))
	    (else
	     (ask place 'exit self)
	     (announce-move name place new-place)
	     (for-each
	      (lambda (p)
		(ask place 'gone p)
		(ask new-place 'appear p))
	      possessions)
	     (set! place new-place)
	     (ask new-place 'enter self))))) )

(define thing
  (let ()
    (lambda (class-message)
      (cond
       ((eq? class-message 'instantiate)
	(lambda (name)
	  (let ((self '()) (possessor 'no-one))
	    (define (dispatch message)
	      (cond
	       ((eq? message 'initialize)
		(lambda (value-for-self)
		  (set! self value-for-self)))
	       ((eq? message 'send-usual-to-parent)
		(error "Can't use USUAL without a parent." 'thing))
	       ((eq? message 'name) (lambda () name))
	       ((eq? message 'possessor) (lambda () possessor))
	       ((eq? message 'type) (lambda () 'thing))
	       ((eq? message 'change-possessor)
		(lambda (new-possessor)
		  (set! possessor new-possessor)))
	       (else (no-method 'thing))))
	    dispatch)))
       (else (error "Bad message to class" class-message))))))

(define-class (ticket number)
  (parent (thing 'ticket)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implementation of thieves for part two
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define *foods* '(pizza potstickers coffee))

(define (edible? thing)
  (member? (ask thing 'name) *foods*))

(define-class (thief name initial-place)
  (parent (person name initial-place))
  (instance-vars
   (behavior 'steal))
  (method (type) 'thief)

  (method (notice person)
    (if (eq? behavior 'run)
	(ask self 'go (pick-random (ask (usual 'place) 'exits)))
	(let ((food-things
	       (filter (lambda (thing)
			 (and (edible? thing)
			      (not (eq? (ask thing 'possessor) self))))
		       (ask (usual 'place) 'things))))
	  (if (not (null? food-things))
	      (begin
	       (ask self 'take (car food-things))
	       (set! behavior 'run)
	       (ask self 'notice person)) )))) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; this next procedure is useful for moving around

(define (move-loop who)
  (newline)
  (print (ask who 'exits))
  (display "?  > ")
  (let ((dir (read)))
    (if (equal? dir 'stop)
	(newline)
	(begin (print (ask who 'go dir))
	       (move-loop who)))))


;; One-way paths connect individual places.

(define (can-go from direction to)
  (ask from 'new-neighbor direction to))


(define (announce-take name thing)
  (newline)
  (display name)
  (display " took ")
  (display (ask thing 'name))
  (newline))

(define (announce-move name old-place new-place)
  (newline)
  (newline)
  (display name)
  (display " moved from ")
  (display (ask old-place 'name))
  (display " to ")
  (display (ask new-place 'name))
  (newline))

(define (have-fit p)
  (newline)
  (display "Yaaah! ")
  (display (ask p 'name))
  (display " is upset!")
  (newline))


(define (pick-random set)
  (nth (random (length set)) set))

(define (delete thing stuff)
  (cond ((null? stuff) '())
	((eq? thing (car stuff)) (cdr stuff))
	(else (cons (car stuff) (delete thing (cdr stuff)))) ))

(define (person? obj)
  (and (procedure? obj)
       (ask obj 'person? )))

(define (thing? obj)
  (and (procedure? obj)
       (ask obj 'thing?)))

; 2E

(define-class (thing name)
 (parent (basic-object))
 (instance-vars (possessor 'no-one))
 (method (thing?) #t)
 (method (type) 'thing)
 (method (change-possessor new-possessor)
   (set! possessor new-possessor)))


(define-class (laptop name)
  (parent  (thing name))
  (method (laptop?) #t)
  (method (connect hotspot-password)
	  ((ask (ask possessor 'place) 'connect selft hotspot-password)))
  (method (surf url)
	  (let ((place (ask possessor 'place))
		       (if (hotspot? place)
			   (ask hotspot  'surf self url)
			   (error "laptop not connected to the hotspot"))))))
