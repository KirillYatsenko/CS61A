(load "3_3-3_4")

(define (make-joint account account-password additional-password)
 (if (not ((account 'validate 'fake) account-password))
     error "wrong provided password"
     (lambda (m provided-password)
       (if (not (eq? provided-password additional-password))
	   error "wrong provided password"
	   (account m account-password)))))
