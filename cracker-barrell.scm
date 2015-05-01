
; Cracker Barrell Search using DFS
; Test: (dfs (list '(1 2 3 4 6 7 8 9 10 11 12 13 14 15)) '(13) '() '())

; a list for holding all legal moves for a equilateral triangle with 15 pegs
(define legal-move (list '(1 2 4) '(1 3 6)
			 '(2 4 7) '(2 5 9)
			 '(3 5 8) '(3 6 10)
			 '(4 2 1) '(4 5 6) '(4 7 11) '(4 8 13)
			 '(5 8 12) '(5 9 14)
			 '(6 3 1) '(6 5 4) '(6 9 13) '(6 10 15)
			 '(7 4 2) '(7 8 9)
			 '(8 9 10) '(8 5 3)
			 '(9 5 2) '(9 8 7)
			 '(10 9 8) '(10 6 3)
			 '(11 7 4) '(11 12 13)
			 '(12 8 5) '(12 13 14)
			 '(13 14 15) '(13 9 6) '(13 12 11) '(13 8 4)
			 '(14 13 12) '(14 9 5)
			 '(15 14 13) '(15 10 6)))

;; helper method to get A-B (set difference) for list A and B
(define (set-difference a b)
  (cond ((null? a) '())
	((member (car a) b) (set-difference (cdr a) b))
	(else (cons (car a) (set-difference (cdr a) b)))))

;; full board for equilateral triangle-15 pegs
(define board '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))

;; helper method to check if val is present in list 
(define (contains? list val)
  (if (null? list) #f
      (or (eq? (car list) val) (contains? (cdr list) val))))

;; initial method to check if board is solvable
(define (check-hole alist)
  (contains? valid-holes (car (set-difference board alist))))

;;helper method
(define (list-append p q)
  (if (null? p)
      q
      (cons (car p) (list-append (cdr p) q))))

;; helper method to delete the item from list and return the new list
(define list-delete-item
  (lambda (item alist)
    (cond
     ((null? alist) '()) 
      ((equal? item (car alist)) (cdr alist))
          (else (cons (car alist) (list-delete-item item (cdr alist)))))))

;; helper method to insert into a list in sorted order
(define sort-push
  (lambda (x y)
    (let ((alist (append (list x) y))) (sort alist <)))
)


;; method to apply move to a state and generate new state
(define (apply-move state move)
  (let* ((nstate (list-delete-item (car move) state))
	 (nstate (list-delete-item (car (cdr move)) nstate))
	 (nstate (sort-push (car (cdr (cdr move))) nstate)))
    nstate
  )
)

;; list of valid moves stored in lom
;; main method to generate a list of moves applicable for a state
(define (gen-moves state)
 (define lom '()) 
  (let* ((holes (set-difference board state)) (curr-st state))
    (for-each (lambda (move)
	(and (contains? curr-st (car move)) (contains? curr-st (car (cdr move))) (contains? holes (car (cdr (cdr move))))
	    (set! lom (append (list move) lom))
	       
	 )
	
	) legal-move)
    ) lom)


;; list that holds the list of children of a given state
;; this method generates a list of states that result from applying
;; all valid moves on a state
(define (get-children state)
  (define loc '())
  ;(define lom '())
  (define lom (gen-moves state))
  (for-each (lambda (move)
	      (set! loc (append (list (apply-move state move)) loc))
	      )
	    lom)
  loc)
	      

;; depth first search for the goal state from a given state
(define (dfs paths old-states new-branches)
  ; (display paths)
  (if (null? paths)
      #f  ;; indicates that the puzzle cannot be solved.
      (let* ((current-state (car paths)))
	(if (equal? current-state '(1 2 3 4 5 6 7 9 10 11 13 14 15)) 
	    (reverse paths)
	    (let* ((adjacent-states (get-children (list current-state)))
		   (new-states (set-difference adjacent-states old-states))
		   (new-branches (append (map (lambda (x) (cons x paths)) new-states) new-branches))
		   (old-states (append (list current-state) old-states)))
	         (if (null? new-branches) 
		     (dfs (cdr paths) old-states new-branches)
		     (let* ((paths (car new-branches))
			    (new-branches (cdr new-branches)))
		       (dfs paths old-states new-branches))))))))


;;check if a state is a goal state or not
(define goal?
  (lambda (state)
   (equal? state (list 1 2 3 4 5 6 7 8 10 11 12 13 15))    ;; solution for '(1 2 3 4 6 7 8 9 10 11 12 13 14 15)
  ))
