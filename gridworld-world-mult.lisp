; The multiagent world of Brenner and Nebel
; =========================================
  
; Rather than directly telling the agents about inaccessible locations,
; we'll create a set of world facts (not known in advance to the agents)
; about which squares are initially occupied. When an agent is initially
; placed at a location, the 'is_occupied' predication for the location 
; is added in the world. 
;
; To enable an agent to see whether it can walk to an adjacent location,
; 'facts-evident-to' (in "gridworld-planning.lisp") has been changed
; to include facts about adjacent locations, and thus about whether or
; not they are occupied. (But in the absence of direct knowledge,
; agents assume, via the CWA, that locations are not occupied.)
;
; The initial, protected 'is_occupied' facts are created from the 
; following, as part of the def-roadmap function in "gridworld-
; definitions.lisp"; so this has to precede the def-roadmap call:

;; the raw list of blocked walls
(defparameter *wall-list-raw* 
  '((Sbb Sbc Sbd Sbe Sbf Scb Scf Sdb Sdf Seb Sef Sfb Sfc Sfd Sfe Sff)
    (Sbh Sbi Sbj Sbk Sbl Sch Scl Sdh Sdl Seh Sel Sfh Sfi Sfj Sfk Sfl) 
    (Sbn Sbo Sbp Sbq Sbr Scn Scr Sdn Sdr Sen Ser Sfn Sfo Sfp Sfq Sfr) 
    (Shb Shc Shd She Shf Sib Sif Sjb Sjf Skb Skf Slb Slc Sld Sle Slf) 
    (Shh Shi Shj Shk Shl Sih Sil Sjh Sjl Skh Skl Slh Sli Slj Slk Sll) 
    (Shn Sho Shp Shq Shr Sin Sir Sjn Sjr Skn Skr Sln Slo Slp Slq Slr)))

(defparameter *blocked-points* (make-block *wall-list-raw*))

(def-roadmap '(Saa Sab Sac Sad Sae Saf Sag Sah Sai Saj Sak Sal Sam San Sao Sap Saq Sar Sas
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
               Sma Smb Smc Smd Sme Smf Smg Smh Smi Smj Smk Sml Smm Smn Smo Smp Smq Smr Sms

               '(
                 (Hor1 Saa 1 Sab 1 Sac 1 Sad 1 Sae 1 Saf 1 Sag 1 Sah 1 Sai 1 Saj 1 Sak 1 Sal 1 Sam 1 San 1 Sao 1 Sap 1 Saq 1 Sar 1 Sas)
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
                 (Ver19 Sas 1 Sbs 1 Scs 1 Sds 1 Ses 1 Sfs 1 Sgs 1 Shs 1 Sis 1 Sjs 1 Sks 1 Sls 1 Sms)
                 )))

; Note: adjacency knowledge is created as part of *roadmap-knowledge*
; in gridworld-definitions.lisp (which is imparted to an agent when
; its knowledge is initialized); but 'is_occupied' facts for both 
; the roadmap and for agent placement are *protected-facts* and
; *world-facts*, not *roadmap-knowledge*, and thus initially unknown
; to the agents.

; From an agent's perspective, all locations are initially not occupied,
; via the CWA.

(def-object 'agent nil)

;   DECLARATIONS OF PROBLEM-SPECIFIC GLOBALS IN gridworld-definitions.lisp
;   (MANY OF WHICH ARE EXPECTED TO BE RESET HERE):
;   (defparameter *n-agents* 4)
;   ^^^^^^^^^^^^^^^^^^^^^^^^^^^
;   (defparameter *roadmap-knowledge* (make-hash-table ...))
;   (defparameter *general-knowledge* nil)
;   (defparameter *extra-initial-knowledge* (make-array *n-agents*))
;   (defparameter *left-comoving-preds* nil)
;   (defparameter *right-comoving-preds* nil)
;   (defparameter *occluded-preds* nil)
;   (defparameter *right-comoving-preds* nil)
;   (defvar *visited-places* (make-array *n-agents*))
;   (defvar *visited-objects* (make-array *n-agents*))
;   (defvar *world-facts* (make-hash-table ...))
;   (defvar *protected-facts* (make-hash-table ...))
;
;   DECLARATIONS OF PROBLEM-SPECIFIC GLOBAL IN gridworld-planning.lisp
;   (BOTH OF WHICH ARE EXPECTED TO BE RESET HERE):
;   (defparameter *operators* nil)
;   (defvar *search-beam* nil)
;


(defparameter *agent-names* '(AG1 AG2 AG3 AG4 AG5 AG6 AG7 AG8 AG9 AG10))
                    ; We use *some* of these; Brenner & Nebel allowed up to 10
(defparameter *agent-array* 
   (make-array *n-agents* :initial-contents 
        (butlast *agent-names* (- 10 *n-agents*))))
(defparameter *agent-positions*
   (make-array *n-agents* :initial-contents 
        '(P13 P35 P41 P51))); ** may be replaced by randomized positions
(defparameter *goals*
   (make-array *n-agents* :initial-contents        
        '(P61 P26 P51 P21))); ** may be replaced by randomized positions

(defparameter *agent-goal-indices*; allows hash access to indices, and 
   (make-hash-table :size 23))     ; hence from an agent to its goal or v.v.
(dotimes (j *n-agents*) 
   (setf (gethash (aref *agent-array* j) *agent-goal-indices*) j)
   (setf (gethash (aref *goals* j) *agent-goal-indices*) j))

; place-object has been altered so as to add 'is-occupied' facts 
; (for agents placed at a point) to *world-facts* and *protected-facts*.
(dotimes (j *n-agents*)
   (place-object (aref *agent-array* j) 'agent 
                 (aref *agent-positions* j) 0 nil 
                 ;(list `(has_goal ,(aref *agent-array* j) ,(aref *goals* j))) 
                 ;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~unnecessary
                 nil
                 nil )); no associated things or attitudes

; (setq *occluded-preds* ; we leave it as nil here
;    '(likes contains is_playable knows is_edible is_potable))
; (setq *right-comoving-preds* '(has)); we leave it as nil here
; (setq *left-comoving-preds* ; we leave it as nil here
;    '(is_in))

;(setq *general-knowledge*
;   '(
;     ((tells ?x (that ?y)) => ?y) ; not needed in this world
;     ((knows ?x (that ?y)) => ?y) ; not needed in this world
;   ))

(setq *operators* '(walk stay-put)); defvar is in gridworld-planning.lisp

; This solves the problem in about 17 seconds (using a 150-step bound
; and with implement-effects uncompiled):
(setq *search-beam* ; defvar is in gridworld-planning.lisp
    (list (cons 4 *operators*) (cons 4 *operators*)
          (cons 3 *operators*) (cons 2 *operators*)))

; This gets AG3 trapped at P11, with AG4 already at its goal P21:
; (setq *search-beam* ; defvar is in gridworld-planning.lisp
;     (list (cons 4 *operators*) (cons 4 *operators*)
;           (cons 4 *operators*) (cons 4 *operators*)))

; This gets AG3 trapped at P11, with AG4 already at its goal P21:
; (setq *search-beam* ; defvar is in gridworld-planning.lisp
;    (list (cons 4 *operators*) (cons 3 *operators*)
;          (cons 3 *operators*) (cons 2 *operators*)
;          (cons 2 *operators*) (cons 1 *operators*)))

; This solved the problem in 10 seconds, when run with at most 200 steps,
; with implement-effects uncompiled:
; (setq *search-beam* ; defvar is in gridworld-planning.lisp
;    (list (cons 4 *operators*) (cons 4 *operators*)))

; This succeeded in 13 sec (with much more meandering, but much
; higher speed), with "implements-effects.lisp" compiled.
; With the latter uncompiled, it finished in 8 seconds, but
; with AG3 trapped at P11 by AG4 at P21.
; (setq *search-beam* ; defvar is in gridworld-planning.lisp
;    (list (cons 4 *operators*)))

; The only action with a positive value is staying-put at the goal
; location, though an agent may also stay put when not at its goal
; and unable to move (all adjacent squares occupied). Note: The
; agent must be able to perceive adjacent square occupancy -- on the
; spot, not in advance (so something like (allows_access_to P33 P34) 
; needs to be updated in the actual world when agents move). One change
; from the previous code this requires is that the location itself
; must be added to the 'objects-colocated-with' output, and hence to
; the 'local-objects' variable in that function. This can be done by
; saying that each location is_at itself. Also 'notice-new-local-facts 
; is dependent on 'is_at' predications, so I again this indicates
; the need to say that each location is_at itself. This is now done 
; in "gridworld-definitions.lisp".
;
(setq walk 
; ?x goes from point ?y to adjacent point ?z. For ME, the road was
; specified as well, but it doesn't matter here -- only adjacency 
; matters, and whether the target square is occupied. The value
; of walking is positive if ?z is the ?x's goal square.
  (make-op :name 'walk :pars '(?x ?y ?z)
           :preconds '((is_at ?x ?y)
                       (is_adjacent_to ?y ?z)
                       (not (is_occupied ?z)))
           :effects '((is_at ?x ?z) (not (is_occupied ?y))
                      (not (is_at ?x ?y)) (is_occupied ?z))
           :time-required 1
           :value '(reward-for-walk? ?x ?z))); +ve if ?z is ?x's goal, o/w 0

(defun reward-for-walk? (x z)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Positive if the step to z is x's goal, 0 otherwise
  (if (eql (gethash x *agent-goal-indices* )
           (gethash z *agent-goal-indices* )) 10 0))

(setq walk.actual
; ?x goes from point ?y to adjacent point ?z. For ME, the road was
; specified as well, but it doesn't matter here -- only adjacency 
; matters, and whether the target square is occupied. The value
; of walking is positive if ?z is the ?x's goal square.
  (make-op.actual :name 'walk.actual :pars '(?x ?y ?z)
           :startconds '((is_at ?x ?y) (is_adjacent_to ?y ?z)
                         (not (is_occupied ?z)))
           :stopconds '((is_at ?x ?z))
           :deletes '((is_at ?x ?y) (is_occupied ?y))
           :adds '((is_at ?x ?z) (is_occupied ?z))))

(setq stay-put
; Can stay put at any time, but this is dispreferred to walking, unless 
; the goal has been reached (at which point staying put reaps a big reward)
  (make-op :name 'stay-put :pars '(?x ?y)
           :preconds '((is_at ?x ?y))
           :effects nil
           :time-required 1
           :value '(reward-for-stay-put? ?x ?y))); +ve if ?z is ?x's goal, o/w -1

(setq stay-put.actual
; Can stay put at any time, but this is dispreferred to walking, unless 
; the goal has been reached (at which point staying put reaps a big reward)
  (make-op.actual :name 'stay-put.actual :pars '(?x ?y)
                  :startconds '((is_at ?x ?y))
                  :stopconds '((is_at ?x ?y))
                  :deletes nil
                  :adds nil ))

(defun reward-for-stay-put? (x y)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Positive if y is x's goal, -1 otherwise; (much like reward-for-walk?)
  (if (eql (gethash x *agent-goal-indices*)
           (gethash y *agent-goal-indices*)) 10 -1))
