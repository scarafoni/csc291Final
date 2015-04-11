;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this file is for helper functions that need NO code from other gridworld stuff ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; gets a location list from a symbol for a spot
(defun space-loc (space)
  (let* ((string-space (symbol-name space))
         (row (subseq string-space 1 2))
         (col (subseq string-space 2 3)))
    (mapcar (lambda (x) 
              (- (char-code (coerce x 'character)) 64))
            (list row col))))


(defun set-blocked (l) (cond
  ((null l) nil)
  (t (cons (lose-nth (+ 1 (random 14)) (lose-nth (+ 1 (random 15)) (car l)))  (set-blocked (cdr l))))
))

;; gets rid of nth member of list
(defun lose-nth (n list)
  (format t "lose-nth ~a, ~a~%" n list)
  (append (subseq list 0 (1- n)) (nthcdr n list)))

(defun flatten (a) (cond
	((null a) nil)
	((atom (car a)) (cons (car a) (flatten (cdr a))))
	(t (append (flatten (car a)) (flatten (cdr a))))))

(defun make-block (l)
  (flatten (set-blocked l)))
