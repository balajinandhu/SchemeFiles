(define (getreverse li)
  (if (or (equal? (length li) 1) (null? li))
      li
      (append (getreverse (cdr li)) (cons (car li) '()))))

(define (palindrome? li)
  (if (equal? (getreverse li) li)
      #t
      #f))
