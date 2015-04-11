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

;; the raw list of blocked walls
(defparameter *wall-list-raw* 
  '((Sbb Sbc Sbd Sbe Sbf Scb Scf Sdb Sdf Seb Sef Sfb Sfc Sfd Sfe Sff)
    (Sbh Sbi Sbj Sbk Sbl Sch Scl Sdh Sdl Seh Sel Sfh Sfi Sfj Sfk Sfl) 
    (Sbn Sbo Sbp Sbq Sbr Scn Scr Sdn Sdr Sen Ser Sfn Sfo Sfp Sfq Sfr) 
    (Shb Shc Shd She Shf Sib Sif Sjb Sjf Skb Skf Slb Slc Sld Sle Slf) 
    (Shh Shi Shj Shk Shl Sih Sil Sjh Sjl Skh Skl Slh Sli Slj Slk Sll) 
    (Shn Sho Shp Shq Shr Sin Sir Sjn Sjr Skn Skr Sln Slo Slp Slq Slr)))

(defparameter *blocked-points* (make-block *wall-list-raw*))

(def-roadmap 
    '(Saa Sab Sac Sad Sae Saf Sag Sah Sai Saj Sak Sal Sam San Sao Sap Saq Sar Sas
      Sba Sbb Sbc Sbd Sbe Sbf Sbg Sbh Sbi Sbj Sbk Sbl Sbm Sbn Sbo Sbp Sbq Sbr Sbs
      Sca Scb Scc Scd Sce Scf Scg Sch Sci Scj Sck Scl Scm Scn Sco Scp Scq Scr Scs
      Sda Sdb Sdc Sdd Sde Sdf Sdg Sdh Sdi Sdj Sdk Sdl Sdm Sdn Sdo Sdp Sdq Sdr Sds
      Sea Seb Sec Sed See Sef Seg Seh Sei Sej Sek Sel Sem Sen Seo Sep Seq Ser Ses
      Sfa Sfb Sfc Sfd Sfe Sff Sfg Sfh Sfi Sfj Sfk Sfl Sfm Sfn Sfo Sfp Sfq Sfr Sfs
      Sga Sgb Sgc Sgd Sge Sgf Sgg Sgh Sgi Sgj Sgk Sgl Sgm Sgn Sgo Sgp Sgq Sgr Sgs
      Sha Shb Shc Shd She Shf Shg Shh Shi Shj Shk Shl Shm Shn Sho Shp Shq Shr Shs
      Sia Sib Sic Sid Sie Sif Sig Sih Sii Sij Sik Sil Sim Sin Sio Sip Siq Sir Sis
      Sja Sjb Sjc Sjd Sje Sjf Sjg Sjh Sji Sjj Sjk Sjl Sjm Sjn Sjo Sjp Sjq Sjr Sjs
      Ska Skb Skc Skd Ske Skf Skg Skh Ski Skj Skk Skl Skm Skn Sko Skp Skq Skr Sks
      Sla Slb Slc Sld Sle Slf Slg Slh Sli Slj Slk Sll Slm Sln Slo Slp Slq Slr Sls
      Sma Smb Smc Smd Sme Smf Smg Smh Smi Smj Smk Sml Smm Smn Smo Smp Smq Smr Sms)
    
    '((Hor1 Saa 1 Sab 1 Sac 1 Sad 1 Sae 1 Saf 1 Sag 1 Sah 1 Sai 1 Saj 1 Sak 1 Sal 1 Sam 1 San 1 Sao 1 Sap 1 Saq 1 Sar 1 Sas)
      (Hor2 Sba 1 Sbb 1 Sbc 1 Sbd 1 Sbe 1 Sbf 1 Sbg 1 Sbh 1 Sbi 1 Sbj 1 Sbk 1 Sbl 1 Sbm 1 Sbn 1 Sbo 1 Sbp 1 Sbq 1 Sbr 1 Sbs)
      (Hor3 Sca 1 Scb 1 Scc 1 Scd 1 Sce 1 Scf 1 Scg 1 Sch 1 Sci 1 Scj 1 Sck 1 Scl 1 Scm 1 Scn 1 Sco 1 Scp 1 Scq 1 Scr 1 Scs)
      (Hor4 Sda 1 Sdb 1 Sdc 1 Sdd 1 Sde 1 Sdf 1 Sdg 1 Sdh 1 Sdi 1 Sdj 1 Sdk 1 Sdl 1 Sdm 1 Sdn 1 Sdo 1 Sdp 1 Sdq 1 Sdr 1 Sds)
      (Hor5 Sea 1 Seb 1 Sec 1 Sed 1 See 1 Sef 1 Seg 1 Seh 1 Sei 1 Sej 1 Sek 1 Sel 1 Sem 1 Sen 1 Seo 1 Sep 1 Seq 1 Ser 1 Ses)
      (Hor6 Sfa 1 Sfb 1 Sfc 1 Sfd 1 Sfe 1 Sff 1 Sfg 1 Sfh 1 Sfi 1 Sfj 1 Sfk 1 Sfl 1 Sfm 1 Sfn 1 Sfo 1 Sfp 1 Sfq 1 Sfr 1 Sfs)
      (Hor7 Sga 1 Sgb 1 Sgc 1 Sgd 1 Sge 1 Sgf 1 Sgg 1 Sgh 1 Sgi 1 Sgj 1 Sgk 1 Sgl 1 Sgm 1 Sgn 1 Sgo 1 Sgp 1 Sgq 1 Sgr 1 Sgs)
      (Hor8 Sha 1 Shb 1 Shc 1 Shd 1 She 1 Shf 1 Shg 1 Shh 1 Shi 1 Shj 1 Shk 1 Shl 1 Shm 1 Shn 1 Sho 1 Shp 1 Shq 1 Shr 1 Shs)
      (Hor9 Sia 1 Sib 1 Sic 1 Sid 1 Sie 1 Sif 1 Sig 1 Sih 1 Sii 1 Sij 1 Sik 1 Sil 1 Sim 1 Sin 1 Sio 1 Sip 1 Siq 1 Sir 1 Sis)
      (Hor10 Sja 1 Sjb 1 Sjc 1 Sjd 1 Sje 1 Sjf 1 Sjg 1 Sjh 1 Sji 1 Sjj 1 Sjk 1 Sjl 1 Sjm 1 Sjn 1 Sjo 1 Sjp 1 Sjq 1 Sjr 1 Sjs)
      (Hor11 Ska 1 Skb 1 Skc 1 Skd 1 Ske 1 Skf 1 Skg 1 Skh 1 Ski 1 Skj 1 Skk 1 Skl 1 Skm 1 Skn 1 Sko 1 Skp 1 Skq 1 Skr 1 Sks)
      (Hor12 Sla 1 Slb 1 Slc 1 Sld 1 Sle 1 Slf 1 Slg 1 Slh 1 Sli 1 Slj 1 Slk 1 Sll 1 Slm 1 Sln 1 Slo 1 Slp 1 Slq 1 Slr 1 Sls)
      (Hor13 Sma 1 Smb 1 Smc 1 Smd 1 Sme 1 Smf 1 Smg 1 Smh 1 Smi 1 Smj 1 Smk 1 Sml 1 Smm 1 Smn 1 Smo 1 Smp 1 Smq 1 Smr 1 Sms)
      
      
      (Ver1 Saa 1 Sba 1 Sca 1 Sda 1 Sea 1 Sfa 1 Sga 1 Sha 1 Sia 1 Sja 1 Ska 1 Sla 1 Sma)
      (Ver2 Sab 1 Sbb 1 Scb 1 Sdb 1 Seb 1 Sfb 1 Sgb 1 Shb 1 Sib 1 Sjb 1 Skb 1 Slb 1 Smb)
      (Ver3 Sac 1 Sbc 1 Scc 1 Sdc 1 Sec 1 Sfc 1 Sgc 1 Shc 1 Sic 1 Sjc 1 Skc 1 Slc 1 Smc)
      (Ver4 Sad 1 Sbd 1 Scd 1 Sdd 1 Sed 1 Sfd 1 Sgd 1 Shd 1 Sid 1 Sjd 1 Skd 1 Sld 1 Smd)
      (Ver5 Sae 1 Sbe 1 Sce 1 Sde 1 See 1 Sfe 1 Sge 1 She 1 Sie 1 Sje 1 Ske 1 Sle 1 Sme)
      (Ver6 Saf 1 Sbf 1 Scf 1 Sdf 1 Sef 1 Sff 1 Sgf 1 Shf 1 Sif 1 Sjf 1 Skf 1 Slf 1 Smf)
      (Ver7 Sag 1 Sbg 1 Scg 1 Sdg 1 Seg 1 Sfg 1 Sgg 1 Shg 1 Sig 1 Sjg 1 Skg 1 Slg 1 Smg)
      (Ver8 Sah 1 Sbh 1 Sch 1 Sdh 1 Seh 1 Sfh 1 Sgh 1 Shh 1 Sih 1 Sjh 1 Skh 1 Slh 1 Smh)
      (Ver9 Sai 1 Sbi 1 Sci 1 Sdi 1 Sei 1 Sfi 1 Sgi 1 Shi 1 Sii 1 Sji 1 Ski 1 Sli 1 Smi)
      (Ver10 Saj 1 Sbj 1 Scj 1 Sdj 1 Sej 1 Sfj 1 Sgj 1 Shj 1 Sij 1 Sjj 1 Skj 1 Slj 1 Smj)
      (Ver11 Sak 1 Sbk 1 Sck 1 Sdk 1 Sek 1 Sfk 1 Sgk 1 Shk 1 Sik 1 Sjk 1 Skk 1 Slk 1 Smk)
      (Ver12 Sal 1 Sbl 1 Scl 1 Sdl 1 Sel 1 Sfl 1 Sgl 1 Shl 1 Sil 1 Sjl 1 Skl 1 Sll 1 Sml)
      (Ver13 Sam 1 Sbm 1 Scm 1 Sdm 1 Sem 1 Sfm 1 Sgm 1 Shm 1 Sim 1 Sjm 1 Skm 1 Slm 1 Smm)
      (Ver14 San 1 Sbn 1 Scn 1 Sdn 1 Sen 1 Sfn 1 Sgn 1 Shn 1 Sin 1 Sjn 1 Skn 1 Sln 1 Smn)
      (Ver15 Sao 1 Sbo 1 Sco 1 Sdo 1 Seo 1 Sfo 1 Sgo 1 Sho 1 Sio 1 Sjo 1 Sko 1 Slo 1 Smo)
      (Ver16 Sap 1 Sbp 1 Scp 1 Sdp 1 Sep 1 Sfp 1 Sgp 1 Shp 1 Sip 1 Sjp 1 Skp 1 Slp 1 Smp)
      (Ver17 Saq 1 Sbq 1 Scq 1 Sdq 1 Seq 1 Sfq 1 Sgq 1 Shq 1 Siq 1 Sjq 1 Skq 1 Slq 1 Smq)
      (Ver18 Sar 1 Sbr 1 Scr 1 Sdr 1 Ser 1 Sfr 1 Sgr 1 Shr 1 Sir 1 Sjr 1 Skr 1 Slr 1 Smr)
      (Ver19 Sas 1 Sbs 1 Scs 1 Sds 1 Ses 1 Sfs 1 Sgs 1 Shs 1 Sis 1 Sjs 1 Sks 1 Sls 1 Sms)))

;; Note that we create some "current facts" about
;; AG that are really about the situation at plaza;
;; this is just a way of ensuring that AG knows these
;; facts right at the outset.
(place-object 'AG 'robot 'Saa 0  
  nil ; no associated-things
  ; current facts
  '((is_scared_to_degree AG 4.0)
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
  '((knows AG (that (can_open crowbar1 blocked_door1)))
    ;merely (knows guru (whether (is_potable juice3))) won't work, because (knows guru ...) is first
    ;deposited into *protected-facts* and *world-facts* via place-object, and then later filtered 
    ;to see if it should be known (added to local facts) to AG in initialize-state-node. And 
    ;guru's knowledge is occluded and so filtered out. So the bug fix for now is that when you want 
    ;to initially assign to AG the knowledge of some other agent's knowledge, you should prefix 
    ;that with `knows AG that', and hence the form (knows AG (that (knows another_agent ...))).
   )
)

;; these have fixed locations for now
(place-object 'zombie1 'zombie 'Sae 0 
	nil ; no associated-things
	; current facts
	'((is_zombie zombie1))
    nil ; propositional attitudes
)

(place-object 'zombie2 'zombie 'Saf 0 
	nil ; no associated-things
	; current facts
	'((is_zombie zombie2))
    nil ; propositional attitudes
)


;(setq *occluded-preds* 
;    '(is_playable knows is_edible is_potable)
; We omit this, as *occluded-preds* is currently already set in 
; "gridworld-definitions.lisp".

(setq *operators* '(walk)) ;;grab)) ;;walk eat answer_user_ynq answer_user_whq sleep drink ask+whether play))
(setq *search-beam*
;(list (cons 3 *operators*) (cons 3 *operators*) (cons 3 *operators*) (cons 3 *operators*) (cons 3 *operators*) ))
	(list (cons 5 *operators*) (cons 4 *operators*) (cons 3 *operators*) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function answer_to_ynq? returns a well-formed formula indicating whether 
;; or not the arg wff is currently in AG's KB, under the closed world 
;; assumption. For example, if AG is currently hungry according to AG's KB,
;; then (is_hungry AG) is returned as the response to 
;; (answer_to_ynq? '(is_hungry AG)); else, (not (is_hungry AG)) is returned.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun answer_to_ynq? (wff)
	(check-yn-fact-in-kb 'NIL wff (state-node-wff-htable *curr-state-node*))
)

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
;; With operator walk, AG walks from point ?x to point ?y on road ?z, with 
;; initial fatigue level ?f, assuming speed of one unit per time step.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;; A* -> MOVE (THIS IS THE MOVE WE CALL IN THE SECOND SPOT)
;;;
(setq walk 
	(make-op :name 'walk :pars '(?x ?y ?z ?f)
	:preconds '((is_at AG ?x)        
				(is_on ?x ?z)        
				(is_on ?y ?z) (point ?y)
				(navigable ?z)
                (is_tired_to_degree AG ?f) )
    :effects '((is_at AG ?y) 
    		   (not (is_at AG ?x))
               ;(is_tired_to_degree AG (+ ?f 0.5))
               (is_tired_to_degree AG (+ ?f (* 0.5 (distance_from+to+on? ?x ?y ?z))))  
               (not (is_tired_to_degree AG ?f)) )
    :time-required '(distance_from+to+on? ?x ?y ?z)
    :value '(- 3 ?f)
    )
)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With operator walk.actual, AG walks from point ?x to point ?y on road ?z,  
;; with initial fatigue level ?f, assuming speed of one unit per time step.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq walk.actual 
	(make-op.actual :name 'walk.actual :pars '(?x ?y ?z ?f)
	:startconds '((is_at AG ?x)        
				  (is_on ?x ?z)        
				  (is_on ?y ?z) (point y)
				  (navigable ?z)
                  (is_tired_to_degree AG ?f) )
    :stopconds '((not (navigable ?z)) 
    			 (is_at AG ?y) )
    :deletes '((is_at AG ?#1)
    		   (is_tired_to_degree AG ?#2))
    :adds '((is_at AG (the_pt+units_from+towards+on_road? (* 1 (elapsed_time?)) ?x ?y ?z))
    		(is_at AG (the_pt+units_from+towards+on_road? (- (distance_from+to+on? ?x ?y ?z) (* 1 (elapsed_time?))) ?y ?x ?z))
    	    (is_on (the_pt+units_from+towards+on_road? (* 1 (elapsed_time?)) ?x ?y ?z) ?z)
    	    (is_on (the_pt+units_from+towards+on_road? (- (distance_from+to+on? ?x ?y ?z) (* 1 (elapsed_time?))) ?y ?x ?z) ?z)
    		(is_tired_to_degree AG (+ ?f (* 0.5 (elapsed_time?)))) )
    )
)
