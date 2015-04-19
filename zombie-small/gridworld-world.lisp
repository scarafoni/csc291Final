 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Name: gridworld-world.lisp
;; This is the sample gridworld you can experiment with to familiarize 
;; yourself with the framework. Note that when creating your new gridworld,
;; you should modify the functions state-value and expected-value in
;; gridworld-planning.lisp to reflect the likes and dislikes of your agent.
;; 
;; Author: Daphne Liu
;; Date: Jan. 2010 by Daphne Liu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun getZMoves (s) 
  (let ((l  (gethash '(nil s nil nil) *roadmap-knowledge*)))
    (print (car l))
    (cadddr (nth (random (length l)) l))))

;; (def-roadmap '(SAA SAB SAC 
;; 		SBA SBB SBC 
;; 		SCA SCB SCC) 
;; '(("r1" SAA 1 SAB) ("r2" SAB 1 SAA) ("r3" SAB 1 SAC) ("r4" SAC 1 SAB)
;;  ("r5" SBA 1 SBB) ("r6" SBB 1 SBA) ("r7" SBB 1 SBC) ("r8" SBC 1 SBB)
;;  ("r9" SCA 1 SCB) ("r10" SCB 1 SCA) ("r11" SCB 1 SCC) ("r12" SCC 1 SCB)
;;  ("r13" SAA 1 SBA) ("r14" SBA 1 SAA) ("r15" SBA 1 SCA)
;;  ("r16" SCA 1 SBA) ("r17" SAB 1 SBB) ("r18" SBB 1 SAB)
;;  ("r19" SBB 1 SCB) ("r20" SCB 1 SBB) ("r21" SAC 1 SBC)
;;  ("r22" SBC 1 SAC) ("r23" SBC 1 SCC) ("r24" SCC 1 SBC)) )



;; (def-roadmap '(SAA SAB SAC SAD SAE SAF
;;                SBA SBB SBC SBD SBE SBF
;;                SCA SCB SCC SCD SCE SCF
;;                SDA SDB SDC SDD SDE SDF
;;                SEA SEB SEC SED SEE SEF
;;                SFA SFB SFC SFD SFE SFF)
;;     '(("r1" SAA 1 SAB) ("r2" SAB 1 SAA) ("r3" SAB 1 SAC) ("r4" SAC 1 SAB)
;;       ("r5" SBA 1 SBB) ("r6" SBB 1 SBA) ("r7" SBB 1 SBC) ("r8" SBC 1 SBB)
;;       ("r9" SCA 1 SCB) ("r10" SCB 1 SCA) ("r11" SCB 1 SCC) ("r12" SCC 1 SCB)
      
;;       ("r13" SAA 1 SBA) ("r14" SBA 1 SAA) ("r15" SBA 1 SCA)
;;       ("r16" SCA 1 SBA) ("r17" SAB 1 SBB) ("r18" SBB 1 SAB)
;;       ("r19" SBB 1 SCB) ("r20" SCB 1 SBB) ("r21" SAC 1 SBC)
;;       ("r22" SBC 1 SAC) ("r23" SBC 1 SCC) ("r24" SCC 1 SBC)
      
      
;;       ("r25" SAD 1 SAE) ("r26" SAE 1 SAD) ("r27" SAE 1 SAF) ("r28" SAF 1 SAE)
;;       ("r29" SBD 1 SBE) ("r30" SBE 1 SBD) ("r31" SBE 1 SBF) ("r32" SBF 1 SBE)
;;       ("r33" SCD 1 SCE) ("r34" SCE 1 SCD) ("r35" SCE 1 SCF) ("r36" SCF 1 SCE)
      
;;       ("r37" SAD 1 SBD) ("r38" SBD 1 SAD) ("r39" SBD 1 SCD)
;;       ("r40" SCD 1 SBD) ("r41" SAE 1 SBE) ("r42" SBE 1 SAE)
;;       ("r43" SBE 1 SCE) ("r44" SCE 1 SBE) ("r45" SAF 1 SBF)
;;       ("r46" SBF 1 SAC) ("r47" SBF 1 SCF) ("r48" SCF 1 SBF)
      
      
;;       ("r49" SDA 1 SDB) ("r50" SDB 1 SDA) ("r51" SDB 1 SDC) ("r52" SDC 1 SDB)
;;       ("r53" SEA 1 SEB) ("r54" SEB 1 SEA) ("r55" SEB 1 SEC) ("r56" SEC 1 SEB)
;;       ("r57" SFA 1 SFB) ("r58" SFB 1 SFA) ("r59" SFB 1 SFC) ("r60" SFC 1 SFB)
      
;;       ("r61" SDA 1 SEA) ("r62" SEA 1 SDA) ("r63" SEA 1 SFA)
;;       ("r64" SFA 1 SEA) ("r65" SDB 1 SEB) ("r66" SEB 1 SDB)
;;       ("r67" SEB 1 SFB) ("r68" SFB 1 SEB) ("r69" SDC 1 SEC)
;;       ("r70" SEC 1 SDC) ("r71" SEC 1 SFC) ("r72" SFC 1 SEC)
      
      
;;       ("r73" SDD 1 SDE) ("r74" SDE 1 SDD) ("r75" SDE 1 SDF) ("r76" SDF 1 SDE)
;;       ("r77" SED 1 SEE) ("r78" SEE 1 SED) ("r79" SEE 1 SEF) ("r80" SEF 1 SEE)
;;       ("r81" SFD 1 SFE) ("r82" SFE 1 SFD) ("r83" SFE 1 SFF) ("r84" SFF 1 SFE)
      
;;       ("r85" SDD 1 SED) ("r86" SED 1 SDD) ("r87" SED 1 SFD)
;;       ("r88" SFD 1 SED) ("r89" SDE 1 SEE) ("r90" SEE 1 SDE)
;;       ("r91" SEE 1 SFE) ("r92" SFE 1 SEE) ("r93" SDF 1 SEF)
;;       ("r94" SEF 1 SDF) ("r95" SEF 1 SFF) ("r96" SFF 1 SEF)

;;       ("r97" SBC 1 SBD) ("r98" SBD 1 SBC) ("r99" SCE 1 SDE) ("r100" SDE 1 SCE)
;;       ("r101" SCB 1 SDB) ("r102" SDB 1 SCB) ("r103" SEC 1 SED) ("r104" SED 1 SEC)))


(def-roadmap '(                SAE SAF
                   SBB SBC SBD SBE SBF
                   SCB SCC SCD SCE SCF
                   SDB SDC     SDE    
                   SEB SEC SED SEE    
                   SFB         SFE    )
    '(                                                                   
                                        ("r7" SBB 1 SBC) ("r8" SBC 1 SBB)
                                         ("r11" SCB 1 SCC) ("r12" SCC 1 SCB)
      
                                                           
                                                           
      ("r19" SBB 1 SCB) ("r20" SCB 1 SBB)                  
                        ("r23" SBC 1 SCC) ("r24" SCC 1 SBC)
      
      
                                          ("r27" SAE 1 SAF) ("r28" SAF 1 SAE)
      ("r29" SBD 1 SBE) ("r30" SBE 1 SBD) ("r31" SBE 1 SBF) ("r32" SBF 1 SBE)
      ("r33" SCD 1 SCE) ("r34" SCE 1 SCD) ("r35" SCE 1 SCF) ("r36" SCF 1 SCE)
      
                                          ("r39" SBD 1 SCD)
      ("r40" SCD 1 SBD) ("r41" SAE 1 SBE) ("r42" SBE 1 SAE)
      ("r43" SBE 1 SCE) ("r44" SCE 1 SBE) ("r45" SAF 1 SBF)
                        ("r47" SBF 1 SCF) ("r48" SCF 1 SBF)
      
      
                                          ("r51" SDB 1 SDC) ("r52" SDC 1 SDB)
                                          ("r55" SEB 1 SEC) ("r56" SEC 1 SEB)
                                                                             
      
                                                           
                        ("r65" SDB 1 SEB) ("r66" SEB 1 SDB)
      ("r67" SEB 1 SFB) ("r68" SFB 1 SEB) ("r69" SDC 1 SEC)
      ("r70" SEC 1 SDC)                                    
      
      
                                                                             
      ("r77" SED 1 SEE) ("r78" SEE 1 SED)                                    
                                                                             
      
                                                           
                        ("r89" SDE 1 SEE) ("r90" SEE 1 SDE)
      ("r91" SEE 1 SFE) ("r92" SFE 1 SEE)                  
                                                           

      ("r97" SBC 1 SBD) ("r98" SBD 1 SBC) ("r99" SCE 1 SDE) ("r100" SDE 1 SCE)
      ("r101" SCB 1 SDB) ("r102" SDB 1 SCB) ("r103" SEC 1 SED) ("r104" SED 1 SEC)))

(def-object 'survivor '(is_animate can_think))
(def-object 'crowbar '(can_open_door))
(def-object 'door '(is_inanimate is_door is_closed))
(def-object 'zombie '(is_smelly is_zombie is_fat))

;(def-object 'expert '(is_animate can_talk))
;(def-object 'instrument '(is_inanimate is_playable))
; (def-object 'juice '(is_inanimate is_potable (has_cost 2.0)))
;(def-object 'pizza '(is_inanimate is_edible (has_cost 5.0)))


;; Note that we create some "current facts" about
;; AG that are really about the situation at plaza;
;; this is just a way of ensuring that AG knows these
;; facts right at the outset.
(place-object 'AG 'survivor 'SBB 0  
  nil ; no associated-things
  ; current facts
  '((is_scared_to_degree AG 4.0)
    (is_at door1 SFE)
    (is_at crowbar1 SCD)
    (can_open_door crowbar1)
    (can_open_door crowbar2)
    (is_at crowbar2 SFB)
    (is_at zombie1 SBF)
    (is_closed door1)
    (is_door door1)
    (is_zombie zombie1)
    ;;(is_hungry_to_degree AG 4.0)
	;(is_thirsty_to_degree AG 2.0)
    ;;(is_tired_to_degree AG 0.0)
    ;(can_talk guru)
    ;(is_at guru grove)
    ;(is_at juice3 plaza)  
     ;Note that right after the call to function initialize-state-node, 
     ;AG knows (is_edible pizza3) and (is_playable piano2). The reason is
     ;AG knows the types of pizza3 and piano2 colocated with AG at home,
     ;and AG does forward inference using its initial knowledge in 
     ;conjunction with *general-knowledge* to derive the knowledge 
     ;of the type-specific properties of pizza3 and piano2. All this 
     ;occurs despite the general occlusiveness of the predicates 
     ;is_edible and is_playable as specified by *occluded-preds*.
     ;This behavior is acceptable. To prevent AG from knowing about 
     ;the edibility of pizza3 and playability of piano2 at the outset,
     ;one would need to specify general inference rules from AG to use,
     ;separate from *general-knowledge*. But for simplicity, we specify
     ;only *general-knowledge* as the inference rules both known in the 
     ;simulated world and used by AG.
  ) 
  ; propositional attitudes
  '((knows AG (whether (can_open_door crowbar1))) 
    (knows AG (whether (is_door door1)))
    ;;(knows AG (whether (is_playable piano2)))
    ;(knows AG (whether (is_edible pizza3)))
    ;(knows AG (that (knows guru (whether (is_potable juice3))))) 
    ;merely (knows guru (whether (is_potable juice3))) won't work, because (knows guru ...) is first
    ;deposited into *protected-facts* and *world-facts* via place-object, and then later filtered 
    ;to see if it should be known (added to local facts) to AG in initialize-state-node. And 
    ;guru's knowledge is occluded and so filtered out. So the bug fix for now is that when you want 
    ;to initially assign to AG the knowledge of some other agent's knowledge, you should prefix 
    ;that with `knows AG that', and hence the form (knows AG (that (knows another_agent ...))).
   )
)

(place-object 'crowbar1 'crowbar 'SCD 0
              nil
              ; current facts
              '((can_open_door crowbar1)
               )
              nil
)

(place-object 'crowbar2 'crowbar 'SFB 0
              nil
              ; current facts
              '((can_open_door crowbar2)
               )
              nil
)

(place-object 'zombie1 'zombie 'SBF 0
		nil
		;current facts
		'((is_zombie zombie1))
		nil
)

(place-object 'door1 'door 'SFE 0
    nil
    ; current facts
    '((is_inanimate door1)
      (is_door door1)
      (is_closed door1)
     )
    nil
)

;; (place-object 'pizza3 'pizza 'home 0 
;; 	nil ; no associated-things
;; 	; current facts
;; 	'((is_edible pizza3) 
;; 	 )
;;     nil ; propositional attitudes
;; )

;; (place-object 'juice3 'juice 'plaza 0 
;; 	nil ; no associated-things
;; 	; current facts
;; 	'((is_potable juice3) 
;; 	 )
;;     nil ; propositional attitudes
;; )

;; (place-object 'piano2 'instrument 'home 0 
;; 	nil ; no associated-things
;; 	'((is_playable piano2)
;; 	 )
;;     nil ; propositional attitudes
;; )

;; (place-object 'guru 'expert 'grove 0 
;; 	nil ; no associated-things
;;     nil ; no current facts
;;     ; propositional attitudes
;;     '((knows guru (whether (is_potable juice3)))
;;      )
;; )

;(setq *occluded-preds* 
;    '(is_playable knows is_edible is_potable)
; We omit this, as *occluded-preds* is currently already set in 
; "gridworld-definitions.lisp".

;; (setq *operators* '(walk eat get_killed answer_user_ynq answer_user_whq sleep drink ask+whether play))
(setq *operators* '(walk grab_crowbar get_killed open_door answer_user_ynq answer_user_whq))
(setq *search-beam*
;(list (cons 3 *operators*) (cons 3 *operators*) (cons 3 *operators*) (cons 3 *operators*) (cons 3 *operators*) ))
	(list (cons 3 *operators*))); (cons 2 *operators*) (cons 2 *operators*) (cons 2 *operators*) (cons 2 *operators*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function answer_to_ynq.actual? returns a well-formed formula indicating  
;; whether the arg wff is currently in AG's KB, under the closed world 
;; assumption. In addition, the answer is translated into a proper English 
;; sentence and printed on screen.  For example, if AG is currently hungry 
;; according to AG's KB, then (is_hungry AG) is returned as the response to 
;; (answer_to_ynq.actual? '(is_hungry AG)), and ``AG is hungry'' without the 
;; double quotes is printed.  Otherwise, (not (is_hungry AG)) is 
;; returned and ``it is not the case that AG is hungry'' is printed without 
;; the double quotes.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun answer_to_ynq.actual? (wff)
	(check-yn-fact-in-kb 'T wff (state-node-wff-htable *curr-state-node*))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function answer_to_whq? returns a collection of well-formed formula(s) 
;; as the answer to the arg wff reflecting what are currently in AG's KB, 
;; under the closed world assumption. Arg wff is a wh-question that has 
;; variables prefixed with ? appearing in slots filled by wh-words.  
;; For example, if AG likes only APPLE1 and BANANA2 according to AG's KB,
;; then ((likes AG APPLE1) (likes AG BANANA2)) is returned as response to 
;; (answer_to_whq? '(likes AG ?wh)). If no answer is found, 
;; then '(not (knows (AG the-answer))) is returned.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun answer_to_whq? (wff)
	(check-whq-answer-in-kb 'NIL wff (state-node-wff-htable *curr-state-node*))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function answer_to_whq.actual? returns a collection of well-formed 
;; formula(s) as the answer to the arg wff reflecting what are currently in 
;; AG's KB, under the closed world assumption. Arg wff is a wh-question 
;; with variables prefixed with ? appearing in slots filled by wh-words.  
;; For example, if AG likes only APPLE1 and BANANA2 according to AG's KB,
;; ((likes AG APPLE1) (likes AG BANANA2)) is returned as the response to 
;; (answer_to_whq.actual? '(likes AG ?wh)), and ``AG likes APPLE1'' and ``AG likes 
;; BANANA2'' without double quotes are printed on two lines.  If no answer 
;; is found, '(not (knows (AG the-answer))) is returned and ``it is not the 
;; case that AG knows the answer'' without the double quotes is printed .
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun answer_to_whq.actual? (wff)
	(check-whq-answer-in-kb 'T wff (state-node-wff-htable *curr-state-node*))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With operator answer_user_ynq, AG answers the yes-no question ?q asked 
;; by USER.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq answer_user_ynq 
      (make-op :name 'answer_user_ynq :pars '(?q)
        :preconds '( (wants USER (that (tells AG USER (whether ?q)))) )
        :effects '( (not (wants USER (that (tells AG USER (whether ?q)))))
                    (knows USER (that (answer_to_ynq? ?q)))
			  		)
        :time-required 1
        :value 10
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With operator answer_user_ynq.actual, AG answers the yes-no question 
;; ?q asked by USER.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq answer_user_ynq.actual 
	(make-op.actual :name 'answer_user_ynq.actual :pars '(?q)
	:startconds '( (wants USER (that (tells AG USER (whether ?q)))) )
	:stopconds '( (not (wants USER (that (tells AG USER (whether ?q))))) )
	:deletes '( (wants USER (that (tells AG USER (whether ?q)))) )
	:adds '( ;(knows USER (that (answer_to_ynq?.actual ?q)))				
					 (says+to+at_time AG (that (answer_to_ynq.actual? ?q)) USER (current_time?))
					 (not (wants USER (that (tells AG USER (whether ?q)))))
		   	 )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With operator answer_user_whq, AG answers the wh-question ?q asked by 
;; USER.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq answer_user_whq 
	(make-op :name 'answer_user_whq :pars '(?q)
	:preconds '( (wants USER (that (tells AG USER (answer_to_whq ?q)))) )
	:effects '( (not (wants USER (that (tells AG USER (answer_to_whq ?q)))))
				(knows USER (that (answer_to_whq? ?q)))
			  )
	:time-required 1
	:value 10
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With operator answer_user_whq.actual, AG answers the wh-question ?q 
;; asked by USER.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq answer_user_whq.actual 
	(make-op.actual :name 'answer_user_whq.actual :pars '(?q)
	:startconds '( (wants USER (that (tells AG USER (answer_to_whq ?q)))) )
	:stopconds '( (not (wants USER (that (tells AG USER (answer_to_whq ?q))))) )
	:deletes '( (wants USER (that (tells AG USER (answer_to_whq ?q)))) )
	:adds	'( ;(knows USER (that (answer_to_whq.actual? ?q)))				
			   (says+to+at_time AG (that (answer_to_whq.actual? ?q)) USER (current_time?))
			   (not (wants USER (that (tells AG USER (answer_to_whq ?q)))))
			 )
	)	
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operator fire.actual is the exogenous fire operator.  As long as there 
;; is no rain, a spontaneous fire has a 5% chance of starting; once 
;; it has started, it has a 50% chance of stopping, and it also goes out 
;; as soon as there is rain.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq zombie-move.actual 
;; 	(make-op.actual :name 'zombie-move.actual :pars '(?z ?x ?y ?p) ; zombie at ?x
;;     :startconds '((is_at ?z ?x) 
;; 		  (is_zombie ?z) 
;; 		  ;(= 0 (random 1)) 
;; 		  (is_on ?x ?p)
;; 		  (is_on ?y ?p)
;; 		  (navigable ?p))
;; 					; 50% chance of zombie walking
;;     :starredStopConds '(T) ; 100% chance of stopping after starting
		       
;;     :starredDeletes '((is_at ?z ?x))
;;     :starredAdds '((is_at ?z ?y) )
;;     :deletes '()
;;     :adds '();; (knows AG (whether (is_at ?z ?y)))) 
;;     )
;; )

(setq zombie-move.actual 
	(make-op.actual :name 'zombie-move.actual :pars '(?z ?x ?y) ; zombie at ?x
    :startconds '((is_at ?z ?x) 
		  (is_zombie ?z)) 
		  ;(= 0 (random 1)) 
					; 50% chance of zombie walking
    :starredStopConds '(T) ; 100% chance of stopping after starting
		       
    :starredDeletes '((is_at ?z ?x))
    :starredAdds '((is_at ?z ?y) )
    :deletes '()
    :adds '();; (knows AG (whether (is_at ?z ?y)))) 
    )
)
;;(defun get_adjacent_squares? (x y z)
;;  (let (result pt1 pt2 units index1 index2 str)
 ;;   (if (and (evalFunction Predicate (cons 'point (list x))) (evalFunctionPredicate (cons 'point (list y)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This evaluation function returns the numeric distance from location arg
;; x to location arg y along the path arg z. This function is called by 
;; functions walk.actual and walk.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun distance_from+to+on? (x y z)
	(let	(result pt1 pt2 units index1 index2 str)
			; If both x and y are named road points, simply do a look-up.
		(if (and (evalFunctionPredicate (cons 'point (list x))) (evalFunctionPredicate (cons 'point (list y))))
			(dolist (triple (get x 'next))
				(when (and (eq z (first triple)) (eq y (second triple)))
					(setq result (third triple))
					
					(return-from distance_from+to+on? result)
				)
			)	
			; Otherwise, x is of the form (the_pt+units_from+towards+on_road? ?d ?a ?b ?r), 
			; and parse the result to get the distance.
			(progn
				(if (atom x)
					(setq str (string x))
					(setq str (apply (car x) (cdr x))); (string x))
				)
				(setq index1 (search "PT_" str))
				(setq index2 (search "_UNITS" str))
				(setq units (parse-integer (subseq str (+ index1 3) index2)))
				(setq index1 (search "FROM_" str))
				(setq index2 (search "_TOWARDS" str))
				(setq pt1 (INTERN (string-upcase (subseq str (+ index1 5) index2))))
				(setq index1 (+ index2 9))
				(setq index2 (search "_ON" str))
				(setq pt2 (INTERN (string-upcase (subseq str index1 index2))))
				(if (and (eq pt1 x) (eq pt2 y))
					(return-from distance_from+to+on? (- (distance_from+to+on? pt1 pt2 z) units))
					(return-from distance_from+to+on? units)
				)
			)
		)
	)
)




;; pick up an object
(setq grab_crowbar
	(make-op :name 'grab_crowbar :pars '(?x ?y) ; level of hunger ?h
	:preconds '( (not (has AG ?x)) 
				 (is_at AG ?y) 
				 (is_at ?x ?y) 
				 (can_open_door ?x))
				 ;;(knows AG (whether (can_open_door ?x))))
	:effects '( (is_scared_to_degree AG 2.0) (has AG ?x))
	:time-required 1
	:value '30
	)
)

(setq grab_crowbar.actual
	(make-op.actual :name 'grab_crowbar.actual :pars '(?x ?y) ; level of hunger ?h
	:startconds '((not (has AG ?x)) 
				 (is_at AG ?y) 
				 (is_at ?x ?y) 
				 (can_open_door ?x))
    :stopconds '((has AG ?x))
	:deletes '()
    :adds '((has AG ?x))
	)
)

;; opens a door
(setq open_door
    (make-op :name 'open_door :pars '(?x ?y ?c) ;door x is at y and ag has crowbar c
	:preconds '( (has AG ?c) 
                ;(is_closed ?x)
		(not (is_open ?x))
                (can_open_door ?c)
                (is_at AG ?y) 
                (is_at ?x ?y) 
                (is_door ?x))
	:effects '( (is_scared_to_degree AG 0.0) (is_safe AG) );(is_open ?x))
	:time-required 1
	:value '50
	)
)

(setq open_door.actual
	(make-op.actual :name 'open_door.actual :pars '(?x ?y ?c) ; level of hunger ?h
	:startconds '((has AG ?c) 
                  (is_closed ?x)
                  (is_at AG ?y) 
                  (is_at ?x ?y) 
                  (can_open_door ?c))
    :stopconds '((is_open ?x))
	:deletes '()
    :adds '((is_safe AG) (is_open ?x))
	)
)

;;This kills the man
(setq get_killed
	(make-op :name 'get_killed :pars '(?x ?z)
		:preconds '(
			(is_zombie ?z)
			(is_at AG ?x)
			(is_at ?z ?x))
		:effects '((is_dead AG))
		:time-required 1
		:value '-100
	)
)

(setq get_killed.actual
	(make-op.actual :name 'get_killed.actual :pars '(?x ?z) 
		:startconds '(
			(is_at AG ?x)
			(is_at ?z ?x)
			(is_zombie ?z))
		:stopconds '((is_dead AG))
		:deletes '()
		:adds '((is_dead AG))
	)
)


      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With operator walk, AG walks from point ?x to point ?y on road ?z, with 
;; initial fatigue level ?f, assuming speed of one unit per time step.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq walk 
	(make-op :name 'walk :pars '(?x ?y ?z); ?f)
	:preconds '((is_at AG ?x)        
				(is_on ?x ?z)        
				(is_on ?y ?z) (point ?y)
				(navigable ?z))
                ;;(is_tired_to_degree AG ?f) )
    :effects '((is_at AG ?y) 
    		   (not (is_at AG ?x)))
               ;(is_tired_to_degree AG (+ ?f 0.5))
               ;(is_tired_to_degree AG (+ ?f (* 0.5 (distance_from+to+on? ?x ?y ?z))))  
               ;(not (is_tired_to_degree AG ?f)) )
    :time-required '(distance_from+to+on? ?x ?y ?z)
    :value (if '(is_at zombie1 ?y) -10 -0.1);(- 3 ?f)
    )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With operator walk.actual, AG walks from point ?x to point ?y on road ?z,  
;; with initial fatigue level ?f, assuming speed of one unit per time step.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq walk.actual 
	(make-op.actual :name 'walk.actual :pars '(?x ?y ?z); ?f)
	:startconds '((is_at AG ?x)        
				  (is_on ?x ?z)        
				  (is_on ?y ?z) (point y)
				  (navigable ?z))
                  ; (is_tired_to_degree AG ?f) )
    :stopconds '((not (navigable ?z)) 
    			 (is_at AG ?y) )
    :deletes '((is_at AG ?#1))
    		   ;(is_tired_to_degree AG ?#2))
    :adds '((is_at AG (the_pt+units_from+towards+on_road? (* 1 (elapsed_time?)) ?x ?y ?z))
    		(is_at AG (the_pt+units_from+towards+on_road? (- (distance_from+to+on? ?x ?y ?z) (* 1 (elapsed_time?))) ?y ?x ?z))
    	    (is_on (the_pt+units_from+towards+on_road? (* 1 (elapsed_time?)) ?x ?y ?z) ?z)
    	    (is_on (the_pt+units_from+towards+on_road? (- (distance_from+to+on? ?x ?y ?z) (* 1 (elapsed_time?))) ?y ?x ?z) ?z))
    		;;(is_tired_to_degree AG (+ ?f (* 0.5 (elapsed_time?)))) )
    )
)

;;(getZMoves 'SAA)
