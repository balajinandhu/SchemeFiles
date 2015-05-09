(define init_pop '())
(define counter 0)
(define max '())
(define process_list '())
(define local_max '())
(define size_of_pop 123)

;;generate a population of num_pop candidates
(define (gen-population num_pop my-eval-fn)
  ;; call gen-candidate num_pop times
  (if (equal? 0 num_pop) '()
      (let* ((member (gen-candidate 6)))
  (check_counter)
  (sort_by_fitness (cons (list member (my-eval-fn member)) (gen-population (- num_pop 1) my-eval-fn))))
      )
  
  )

;;helper method to check num of calls to my-eval-fn and exit if >= 50000
(define check_counter
  (lambda ()
     
     (if (>= counter 50000)
   (exit)
   (set! counter (+ counter 1)))
 ))

;;helper method to generate an individual with num_features chromosomes
(define (gen-candidate num_features)
  (if (equal? num_features 0) '()
      (cons (+ 1 (random 20)) (gen-candidate (- num_features 1)))))

;;helper method to get n-th index of a list
(define (get-nth index alist)
  (cond ((= 1 index) (car alist))
  (#t (get-nth (- index 1) (cdr alist)))))

;(define (my-eval-fn alist)
;  (lambda (alist)
;      (let ((a (get-nth 1 alist))
;     (b (get-nth 2 alist))
;     (c (get-nth 3 alist))
;     (d (get-nth 4 alist))
;     (e (get-nth 5 alist))
;     (f (get-nth 6 alist)))
; (+ (* 6 a b)
;    (* 4 c d d)
;    (* -3 a a)
;    (* 46 e f)
;    (* -5 f c c)))
;)
;  )

;; method to sort a list in decreasing order
(define (sort_by_fitness alist)
  (sort alist 
  (lambda (a b) (> (car (cdr a)) (car (cdr b))))))

;; choose the first val_n chromosomes from a individual
(define (choose_n alist val_n)
  (if (equal? val_n 0) '()
      (cons (car alist) (choose_n (cdr alist) (- val_n 1)))))

;; choose last (6-val_n) chromosomes from a individual
(define (choose_last alist val_n)
  (reverse (choose_n (reverse alist) (- 6 val_n))))

;; method to take two individuals and mate them with random cross-over point 
(define (mate-candidates alist blist my-eval-fn)
  (let* ((cross_over (+ 1 (random 6)))
   (alist_1 (choose_n (car alist) cross_over))
   (alist_2 (choose_last (car alist) cross_over))
   (blist_1 (choose_n (car blist) cross_over))
   (blist_2 (choose_last (car blist) cross_over))
   (new_alist (append alist_1 blist_2))
   (new_blist (append blist_1 alist_2)))
    (list (list new_alist (my-eval-fn new_alist))
    (list new_blist (my-eval-fn new_blist)))))

;; mutate a population by changing random elements and sort the population
(define (mutate-population pop my-eval-fn)
  (define m_pop '())
  (for-each (lambda (item)
        (check_counter)
        (let* ((m_list (list (mutate-list (car item)))))
    (set! m_pop (append m_pop (list (append m_list (list (my-eval-fn (car m_list))))))))
        
        )
      pop)
  (sort_by_fitness m_pop)

)

;; mutate a individual by selecting random value for a random chromosome  
(define (mutate-list alist)
  (let* ((rand-attr (random 7))
   (rand-val (+ 1 (random 20))))
    (if (or (null? alist) (equal? rand-attr 0))
  alist
  (if (equal? rand-attr 1)
      (append (list rand-val) (cdr alist))
      (append (choose_n alist (- rand-attr 1)) (list rand-val) (choose_last alist rand-attr))))))

;; returns the top two individuals of a sorted population (superior fitness)
(define (choose-individual population)
      
  (if (null? population) '()
  (list (car population) (car (cdr population)))
  ))

;; method to call choose_individual for any size of population
(define (get_ci n population)      
  (cond ((or (equal? n 0) (equal? n 1)) '())
  ((> n 1) (choose-individual population))))
 
;; top-level function to call all the methods
(define (process-generation population my-eval-fn)
  
  (let* ((size_pop (length population))
   (two_list (get_ci size_pop population))
   )
    (if (null? two_list)
  '()
  (set! process_list (append process_list (mate-candidates (car two_list) (car (cdr two_list)) my-eval-fn))))

    (if (>= (length population) 2)
  (process-generation (cdr (cdr population)) my-eval-fn)
  (mutate-population process_list my-eval-fn)
)))


;; wrapper method takes evaluation function as parameter - keeps track of local max, global max and num of iterations
(define search-ga
  (lambda (my-eval-fn)
    
    (do ((i 0 (+ i 1)))  ;; loop varible i - keeps track of iteration
  ((>= counter 49000)  ;; if count to my-eval-fn exceeds 49000, display result (safe call)
   (display "local:")
   (display local_max) (newline)
   (display "global max:")
   (display max) (newline)
   (display "iterations:")
   (display i))
      (cond ((= i 0) (set! process_list '()) ;; first time loop
       (process-generation (gen-population size_of_pop my-eval-fn) my-eval-fn) ;;generate population with size_of_pop
       (set! init_pop process_list))    ;; set intial population with result of process generation
       
      (else    (set! process_list '())  ;; clear temp list
               (process-generation init_pop my-eval-fn) ;; call process generation with previous generation
         (set! init_pop process_list) ;;create new generation of individuals
         (set! local_max (car init_pop)) ;; get local max as best candidate in generation
         ))
      (cond ((null? local_max) (set! local_max (car init_pop)) (set! max local_max)) ;; if local_max empty, set max
      ((> (car (cdr local_max)) (car (cdr max))) (set! max local_max))) ;;if local_max is greater, update max
    ; (display "local:")  
;  (display local_max) (newline)
;  (display "global max:")
;  (display max) (newline)

      )))
