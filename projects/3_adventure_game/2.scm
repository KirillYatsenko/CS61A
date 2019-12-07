; A - it's an object of class person
;
; B - type, neighbors, exits, look-in, appear, enter gone, exit, new-neighbor, add-entry-procedure, add-exit-procedure, remove-entry-procedure, remove-exit-procedure, clear-all-procs
;
; C  
;	>(ask Brian 'place)
;	Returns: a class (just a procedure) with which we can then interac'     
;	
;	>(let ((where (ask Brian 'place)))
;	      (ask where 'name))
;	
;	Returns: peoples-park, because name is a method which returns
;		 name of the place

;	>(ask Peoples-park 'appear bagel)
;	Returns: ERROR since we didn't define Peoples-park variable
;
; D
;	(ask 61a-lab 'appear computer)
;	Explain.: there is no such variable as Durer
;		  'Durer - just a string
;	
;	(computer 'name)
;	Returns: name object
;
; E
;	
;	
