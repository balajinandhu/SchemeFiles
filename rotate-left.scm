(define (rotate-left n li)
  (cond ((or (null? li) (equal? 0 n)) li)
	(#t (rotate-left (- n 1) (append (cdr li) (cons (car li) '()))))))
