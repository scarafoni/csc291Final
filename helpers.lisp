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
