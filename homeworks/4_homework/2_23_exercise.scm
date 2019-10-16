(define (custom-for-each procedure items)
  (define (for-each procedure items dummy)
	  (if (null? items) 
	       (newline)
	       (for-each procedure (cdr items) (procedure (car items)))))

  (for-each procedure items nil))
