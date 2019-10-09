;###################################################################################

; If changing the order of checking base case for an 'cc' recursive procedure,
; it will break the correct returned value since '(= kinds-of-coins 0)' check
; is also true for correct change count
; since this check is before '(= amount 0)' the procedure will return uncorect value

;####################################################################################