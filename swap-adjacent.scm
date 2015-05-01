(define (swap-adjacent li)
  (cond ((null? li) '())
	((equal? (length li) 1) li)
        (#t (append (list (car (cdr li)) (car li)) (swap-adjacent (cdr (cdr li)))))))	     
