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


(setq wall-list-raw '((Sbb Sbc Sbd Sbe Sbf Scb Scf Sdb Sdf Seb Sef Sfb Sfc Sfd Sfe Sff) (Sbh Sbi Sbj Sbk Sbl Sch Scl Sdh Sdl Seh Sel Sfh Sfi Sfj Sfk Sfl) (Sbn Sbo Sbp Sbq Sbr Scn Scr Sdn Sdr Sen Ser Sfn Sfo Sfp Sfq Sfr) (Shb Shc Shd She Shf Sib Sif Sjb Sjf Skb Skf Slb Slc Sld Sle Slf) (Shh Shi Shj Shk Shl Sih Sil Sjh Sjl Skh Skl Slh Sli Slj Slk Sll) (Shn Sho Shp Shq Shr Sin Sir Sjn Sjr Skn Skr Sln Slo Slp Slq Slr)))


(defun set-blocked (l) (cond
  ((null l) nil)
  (t (cons (lose-nth (random 15) (lose-nth (random 16) (car l)))  (set-blocked (cdr l))))
))

(defun lose-nth (n list)
  (append (subseq list 0 (1- n)) (nthcdr n list)))

(defun flatten (a) (cond
	((null a) nil)
	((atom (car a)) (cons (car a) (flatten (cdr a))))
	(t (append (flatten (car a)) (flatten (cdr a))))))