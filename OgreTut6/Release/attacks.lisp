(in-package "KIENEMAN")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; attached-box
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass+ attached-box ()
  ((:iea parent-state)))

(defmethod x ((box attached-box))
  (+ (slot-value box 'x) (x (parent (parent-state box)))))

(defmethod y ((box attached-box))
  (+ (slot-value box 'y) (y (parent (parent-state box)))))


(defun attached-box-v-fighter (ab fighter)
  (let ((state (parent-state ab)))
    (if (linear-tracking state)
	t
      (< (side-dist state) (radius fighter)))))

(defmethod collision :around ((ahb attached-box) (hb2 fighter))
  (and (attached-box-v-fighter ahb hb2) (call-next-method)))

(defmethod collision :around ((hb2 fighter) (ahb attached-box))
  (and (attached-box-v-fighter ahb hb2) (call-next-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rec-attack-box
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass+ rec-attack-box (attached-box default-hitbox mortal)
  ((hit-objects
    :initform nil
    :accessor hit-objects)))

(defmethod initialize-instance :after ((box rec-attack-box) &key)
  (with-accessors ((parent-state parent-state) (display display)) box
    (with-accessors((parent parent)) parent-state
     (push parent (hit-objects box))
     (setf display (make-hit-rectangle (x box) (+ (y box) (/ (height box) 2)) (width box) (height box) "red" *mgr*)))))

(defmethod main-action ((rab rec-attack-box))
  t)

(defmethod animate ((obj rec-attack-box))
  t)

;;If an attacks box hits a generic object, nothing happens.
(defmethod handle-collision ((rab rec-attack-box) thing)
  t)

(defmethod handle-collision (thing (rab rec-attack-box))
  (handle-collision (rab thing)))

(defmethod handle-collision ((rab rec-attack-box) (fighter fighter))
  (when (= 0 (loop for e in (hit-objects rab) count (eq e fighter)))
    ;;When this hitbox has not previously hit the fighter.
    (push fighter (hit-objects rab)) ;Add fighter to list of hit objects.
    (handle-collision rab (state fighter))))

(defmethod handle-collision ((fighter fighter) (rab rec-attack-box))
  (handle-collision (rab fighter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rec-strike-box
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass+ rec-strike-box (rec-attack-box)
  ((:iea damage)
   (:iea hitstun)
   (:ia hitspeed)
   (:ia hitdeccel)
   (:iea blockstun)
   (:ia blockspeed)
   (:ia blockdeccel)))

;;NOTE: The situation of ((state state) (rab rec-strike-box)) does not need
;;to be handled, as the handle-colision method on rec-attack-box and fighter
;;will always pass arguments here in the order of (rab state).
(defmethod handle-collision ((rab rec-strike-box) (state state))
  (with-accessors ((fighter parent)) state
   (decf (hp fighter) (damage rab))		;Hurt him!
   (change-state fighter (make-hitstun rab))))

(defmethod handle-collision ((rab rec-strike-box) (state high-block))
     (with-accessors ((fighter parent)) state		
       (change-state fighter (make-block-stun rab))
       (attack-blocked (parent-state rab) state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Leaves the defender a set distance away from the attacker on a hit.
(defclass+ rec-attack-sdist-box (rec-strike-box)
  ((:iea hitdist)
   (:ia hit-movement-time)
   (:iea min-hitdist)
   (:iea blockdist)
   (:ia block-movement-time)
   (:iea min-blockdist)))

(defclass+ unblockable-box (rec-attack-sdist-box)
  ())

(defstate "grab"
  :supers
  (single-attack-box)

  :slots
  ((grabbed-entity :initform nil)
   (throw-state :initform nil))

  :main-action
  ((format t "~&~a" (get-butterfly-angle))
   (cond

    ((not throw-state)
     (cond
      ((= tpos 3)
       (set-hitbox
	(make-instance 'grab-hitbox
		       :parent-state state
		       :x (* (+ 2.0 radius) (get-direction fighter)) :y 30.0
		       :radius 2.0 :height 25.0)))

      ((and (> tpos 3) (not grabbed-entity))
       (when (not (get-held :a1))
	 (switch-to-state idle)))

      ((and (> tpos 3) grabbed-entity)
       (when (get-pressed (direction-symbol))
	 (setf throw-state :forward)
	 (setf tpos 0)))))

    ;;Can only occur if grabbed entity exists.
    ((equal throw-state :forward)
     (cond
      ((= tpos 5)
       (decf (hp grabbed-entity) 80)
       (change-state grabbed-entity (make-instance 'stunned
						   :duration 15
						   :kb-direction (get-direction parent)
						   :kb-speed 3.0
						   :decceleration 0.25))
       (switch-to-state high-block-stun
			:duration 14
			:kb-direction (opposite (get-direction parent))
			:kb-speed 0.0
			:decceleration 0.0
			:key-buffer key-buffer)))))))

(defclass+ grab-hitbox (rec-attack-box)
  ())

(defun grabbing (rab state)
  (with-accessors ((submitter parent)) state	
    (with-accessors ((parent-state parent-state)) rab
      (setf (grabbed-entity parent-state) submitter)
      (change-state submitter (make-instance 'grabbed
					     :grabber parent-state)))))

(defun clinch-break (ent1 ent2)
  (change-state ent1 (make-instance 'stunned
				    :duration 10
				    :kb-direction (opposite (get-direction ent2))
				    :kb-speed 3.0
				    :decceleration 0.25))
  (change-state ent2 (make-instance 'stunned
				    :duration 10
				    :kb-direction (get-direction ent1)
				    :kb-speed 3.0
				    :decceleration 0.25)))

(defmethod handle-collision ((rab grab-hitbox) (state state))
  (grabbing rab state))

(defmethod handle-collision ((rab grab-hitbox) (state grab))
  (with-accessors ((parent-state parent-state)) rab		
    (let ((hitbox-entity (parent parent-state))
	  (grabbed-entity (grabbed-entity state))
	  (other-entity (parent state)))
      (if (eql grabbed-entity hitbox-entity) ;When two entities grab each other on the same frame.
	  (clinch-break hitbox-entity other-entity)
	(grabbing rab state)))))


(defun make-static-dist-rab (&key
			     (class 'rec-attack-sdist-box)
			     parent-state
			     damage
			     hitstun
			     hit-movement-time
			     hitdist
			     (min-hitdist 0)
			     blockstun
			     block-movement-time
			     blockdist
			     (min-blockdist 0)
			     x y
			     radius height)
  (let* ((hit-deccel (/ (* 2 (float hitdist)) (* hitstun hitstun)))
	 (hit-init-vel (* hit-deccel hitstun))
	 (block-deccel (/ (* 2 (float blockdist)) (* blockstun blockstun)))
	 (block-init-vel (* block-deccel blockstun)))
    (if (not hit-movement-time) (setf hit-movement-time hitstun))
    (if (not block-movement-time) (setf block-movement-time hitstun))
   (make-instance class
		  :parent-state parent-state
		  :damage damage
		  :hitstun hitstun
		  :hit-movement-time hit-movement-time
		  :hitdist hitdist
		  :min-hitdist min-hitdist
		  :blockstun blockstun
		  :block-movement-time block-movement-time
		  :blockdist blockdist
		  :min-blockdist min-blockdist
		  :x x :Y y
		  :radius radius :height height)))



(defun make-hitstun (rab)
  (make-instance 'stunned
		 :duration (hitstun rab)
		 :kb-direction (get-direction (parent (parent-state rab)))
		 :kb-speed (hitspeed rab)
		 :decceleration (hitdeccel rab)))

(defun hit-collision (rab state)
  (with-accessors ((fighter parent)) state
		  (with-accessors ((attack-state parent-state) (hitdist hitdist)
				   (hitstun hitstun) (hitdeccel hitdeccel) (hit-movement-time hit-movement-time)
				   (hitspeed hitspeed) (min-hitdist min-hitdist)) rab
				   (with-accessors ((attacker parent)) attack-state
						   (decf (hp fighter) (damage rab))
						   (let* ((actual-dist (- hitdist (ground-dist fighter attacker)))
							  (actual-dist (if (> actual-dist min-hitdist)
									   actual-dist min-hitdist)))
						     (setf hitdeccel (/ (* 2 (float actual-dist)) (* hit-movement-time hit-movement-time)))
						     (setf hitspeed (* hitdeccel hit-movement-time))
						     (change-state fighter (make-hitstun rab)))))))

(defmethod handle-collision ((rab rec-attack-sdist-box) (state state))
  (hit-collision rab state))

(defun make-block-stun (rab)
  (make-instance 'high-block-stun
		    :duration (blockstun rab)
		    :kb-direction (get-direction (parent (parent-state rab)))
		    :kb-speed (blockspeed rab)
		    :decceleration (blockdeccel rab)))



(defmethod handle-collision ((rab rec-attack-sdist-box) (state high-block))
  (with-accessors
   ((fighter parent)) state
   (with-accessors
    ((attack-state parent-state) (blockdist blockdist)
     (blockstun blockstun) (blockdeccel blockdeccel) (block-movement-time block-movement-time)
     (blockspeed blockspeed) (min-blockdist min-blockdist)) rab
     (with-accessors
      ((attacker parent)) attack-state
      ;;Calculate the actual distance to be pushed back.
      (let* ((actual-dist (- blockdist (ground-dist fighter attacker)))
	     (actual-dist (if (> actual-dist min-blockdist)
			      actual-dist min-blockdist)))
	(setf blockdeccel (/ (* 2 (float actual-dist)) (* block-movement-time block-movement-time)))
	(setf blockspeed (* blockdeccel block-movement-time))
	(change-state fighter (make-block-stun rab))
	(attack-blocked (parent-state rab) state))))))

(defmethod handle-collision ((rab unblockable-box) (state high-block))
  (hit-collision rab state))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstate "jab"

  :supers
  (single-attack-box)

  :slots
  ((tpos-delay :initform 0) ;Used to delay tpos when canceling the attack.
   (cancel-held-at-start :initform nil)
   (forward-speed :initform 0)
   (to-second :initform nil))

  :animation
  single-animation

  :funcs
  ((attack-time 5)
   (end-time 14))

  :main-action
  ((when (and (> tpos-delay 0) (>= tpos attack-time))
     (setf tpos-delay 0)
     (setf tpos 14));Must eval to an integer to avoid type errors.

   (if (get-held :feint)
       (if (< tpos 4)
	   (setf tpos-delay 1)
	 (reset-key-buffer key-buffer))
     (progn
       ;; (when (and ;; (get-held (direction-symbol))
;; 		  (get-pressed :a1))
;; 	 (setf to-second t))
       ))
   (common-transitions)
   (lcase tpos
	  (attack-time (set-hitbox
			(make-static-dist-rab
			 :class 'jab-box
			 :parent-state state
			 :damage 50
			 :hitstun 18
			 :hit-movement-time 14
			 :hitdist 25
			 :blockstun 8
			 :block-movement-time 6
			 :blockdist 34
			 :x (* (get-direction fighter) 22.0) :Y 52.0
			 :radius 7.0 :height 5.0)))
	  ((+ 1 attack-time) (remove-hitbox))
	  (end-time (switch-to-state idle :key-buffer key-buffer)))
   
   (within 12 end-time
	   (when to-second
	     (switch-to-state second-jab))))
  
  :rest
  (progn
   (defclass+ jab-box (rec-attack-sdist-box)
     ())
  
   (defmethod handle-collision ((rab jab-box) (state stunnable))
     (with-accessors ((fighter parent)) state
		     (decf (hp fighter) (damage rab))
		     (set-initial-stun-recovery state 18)))

   (def-statemeth animate ()
     (scale-animation (list 0 attack-time end-time) (list 0 9 20)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstate "second-jab"
  
  :supers
  (single-attack-box)

  :constants
  ((attack-time 10)
   (end-time 28))

  :slots
  ()

  :alt-name
  "straight"

  :animation
  single-animation

  :main-action
  ((lcase tpos
	  (attack-time (set-hitbox (make-static-dist-rab
			   :parent-state state
			   :damage 70
			   :hitstun 15
			   :hitdist 45
			   :blockstun 5
			   :blockdist 40
			   :x (* (get-direction fighter) 24.0) :Y 50.0
			   :radius 5.0 :height 5.0)))
	  
	  ((+ attack-time 2) (remove-hitbox))
	  
	  (end-time (switch-to-state idle :key-buffer key-buffer))))
  
  :rest
  (progn
   (defmethod animate ((sa second-jab))
     (let* ((tp (/ (float (tpos sa)) 60.0))
	    (ani (animation sa))
	    (mtp (if (< tp attack-time)
		     (* 20.0 (/ tp attack-time))
		   (+ (* (/ (- tp attack-time) (- end-time attack-time)) (- 40.0 20.0)) 20.0))))
       (set-time-position ani mtp)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstate "forward-jab"

  :supers
  (single-attack-box)

  :alt-name
  "jab"

  :slots
  ((vel)
   (foot-pos)
   (attacked-time :initform nil))

  :funcs
  ((attack-time 6))

  :main-action
  ((if (not (< (abs vel) 0.15))
     (let ((dash-accel (* (signum vel) (hs-accel vel foot-pos))))
      (decf vel dash-accel)
      (decf foot-pos (abs vel))
      (move-forward vel))
     (setf vel 0.0))

   (cond
    ((and (not attacked-time)
	  (>= tpos attack-time)
	  (< (abs vel) 0.15))
     (setf attacked-time tpos)
     (set-hitbox (make-static-dist-rab 
		  :parent-state state
		  :damage 40
		  :hitstun 15
		  :hitdist 45
		  :blockstun 5
		  :blockdist 40
		  :x (* (get-direction fighter) 25.0) :Y 44.0
		  :radius 6.0 :height 5.0)))
    
    ((and attacked-time (= tpos (+ 1 attacked-time))) (remove-hitbox))
    
    ((and attacked-time (= tpos (+ 20 attacked-time))) (switch-to-state idle :key-buffer key-buffer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstate "roundhouse"

  :hb
  multi-hitbox
  
  :supers
  (single-attack-box)
  
  :slots
  ((forward-speed :initform 0)
   (hip-box :initform nil)
   (blocked :initform nil))

  :funcs
  ((attack-time 14)
   (hook-time 23)
   (block-time (+ 34 (if blocked 4 0)))
   (end-time (+ block-time 3 (if blocked 3 0))))

  :animation
  single-animation

  :entryfunc
  (progn
    (let ((hip (make-uni-box state 0.0 (* 0.4 (height fighter)) 0.65 0.3)))
     (setf (hip-box state) hip)
     (setf (hitbox-list state)
	   (list
	    (make-uni-box state 0.0 0.0 1.0 0.4)
	    hip
	    (make-uni-box state 0.0 (* 0.7 (height fighter)) 0.5 0.3)))))

  :main-action
  (;; (when (get-released :a1)
;;        (set-tapped :a1 :released t))
;;    (when (get-released :a2)
;;      (set-tapped :a2 :released t))

   (common-transitions)

   (when (and (>= tpos hook-time) (get-tapped :a2) (get-held (opposite-symbol)))
     (switch-to-state forward-hook-kick
		      :tpos 0))

   (when (and (>= tpos block-time) (get-held :defense))
     (switch-to-state high-block-stun
		      :duration (* 1.2 (- end-time tpos))
		      :kb-direction 1.0
		      :kb-speed 0.0
		      :decceleration 0.0))

   (when (<= tpos attack-time)
    (setf (radius hip-box) (+ 0.65 (* 1.5 (/ tpos (float attack-time))))))
   
   (lcase tpos
     (attack-time (set-hitbox
	  (make-static-dist-rab
			 :parent-state state
			 :damage 90
			 :hitstun 18
			 :hitdist 46
			 :blockstun 8
			 :blockdist 44
			 :x (* (get-direction fighter) 36.0) :Y 55.0
			 :radius 9.0 :height 8.0)))
     
     ((+ 1 attack-time) (remove-hitbox))
     
     (end-time (switch-to-state idle :key-buffer key-buffer))))

  :rest
  (progn
    (def-statemeth animate ()
      ;; (ccnm)
;;       (let* ((tp (float tpos))
;; 	     (ani animation)
;; 	     (mtp (map-through-range tp (list 0 attack-time end-time) (list 0 12.0 26.0)))
;; 	     (atp (/ (float mtp) 60.0)))
;; 	(set-time-position ani atp))
      (scale-animation (list 0 attack-time end-time) (list 0 12.0 26.0)))

    (def-statemeth attack-blocked (other-state)
      (ccnm)
      (setf blocked t))))



#|
Precondition: This state should only be entered if coming to a halt
is possible from the foot position of the previous state.
|#
(defstate "straight"

  :constants
  ((max-entrance-speed 1.8)
   (extra-speed 1.0)
   (max-power (+ max-entrance-speed extra-speed))
   (prep-end-time 20.0)
   (reco-end-time 40.0)
   (min-speed 0.15))

  :supers
  (single-attack-box)

  :slots
  (;Action can have the following values [:accel  :prep :reco]
   (action :initform :accel)
   ;;These values must be passed into the state!
   (forward-speed)
   (leg-space)
   ;;These values are calculated from the previous set of values
   (accel-time)
   (base-accel)
   (prep-time)
   (punch-time)
   (reco-time)
   (base-damage)
   (max-vel)
   (deccel-max-vel)
   (prep-base-deccel)
   ;;These values are 'live' and modifiable.
   (vel)
   ;;Key buffer.
   ;; (key-buffer :initform nil)
   )

  :animation
  single-animation

  :main-action
  ((let* ((dir (signum vel)))
     (common-transitions)
    (case action
      
     (:accel 
      (if (<= tpos accel-time)
	  (let* ((leg-delta (abs vel))
		 
		 (dash-accel (* dir base-accel (/ (- max-vel leg-delta) max-vel))))
	    (incf leg-space leg-delta)
	    (move-forward (* (abs leg-delta) dir))
	    (incf vel dash-accel))

	(progn
	  ;; (let* ((leg-delta (abs vel))
	  ;; 	    (max-vel 1.9)
	  ;; 	    (dash-accel (* dir 0.18 (/ (- max-vel leg-delta) max-vel))))
	  ;;       (incf leg-space leg-delta)
	  ;;       (move-forward (* (abs leg-delta) dir)))
	  (setf action :prep)
	  (setf tpos 0))))
    
     ;;Continues to advance the punch until it reaches the extended position.
     (:prep
      (let* ((leg-delta (abs vel))
	     (max-vel deccel-max-vel)
	     (dash-accel (* dir prep-base-deccel (/ (- max-vel leg-delta) max-vel))))
       (if (equal (signum (- (- vel dash-accel) (* min-speed dir))) dir)
	   (progn 
	     (decf leg-space leg-delta)
	     (move-forward (* (abs leg-delta) dir))
	     (decf vel dash-accel))
    
	 ;;When the attack box first needs to be deployed and
	 ;;the state transitions in to recovery.
	 ;;(and (equal action :prep) (not (equal (signum (- vel (* min-speed dir))) dir)))
	 ;;Causes the recovery phase to last longer the faster the punch.
	 (progn
	   (setf action :punch)
	   ;;Resets the timer to 0, allowing the hitbox to be destroyed with precision.
	   (setf tpos 0)))))

     (:punch
      (if (<= tpos punch-time)
	  ()
	(progn
	  ;;Sets the hitbox.
	  (set-hitbox
	    (make-instance 'rec-strike-box
			   :parent-state state
			   :damage base-damage
			   :hitstun (+ 2 reco-time)
			   :hitspeed 1.6
			   :hitdeccel 0.08
			   :blockstun (- reco-time 10)
			   :blockspeed 1.7
			   :blockdeccel 0.08
			   :x (* (get-direction fighter) 22.0) :Y 42.0
			   :radius 7.0 :height 6.0))
	  (setf action :reco)
	  (setf tpos 0))))
    
     (:reco
      ;;After the attack box has already been deployed.
      (when (= tpos 2)
	(remove-hitbox))
      (when (> tpos reco-time)
	(switch-to-state idle :key-buffer key-buffer))))

    (format t "Action: ~a ~10T Tpos: ~a ~10T Vel: ~a" action tpos vel)
    (format t "~&************************~%")))
  
  
  :rest
  (progn
    (defmethod initialize-instance :after ((state straight) &key)
      (with-accessors ((forward-speed forward-speed) (accel-time accel-time) (prep-time prep-time) (reco-time reco-time) (punch-time punch-time) (base-damage base-damage)
		       (base-accel base-accel) (prep-base-deccel prep-base-deccel) (max-vel max-vel) (deccel-max-vel deccel-max-vel) (leg-space leg-space) (vel vel)) state
       (let* ((power (+ forward-speed extra-speed))
	      (total-accel (/ (+ leg-space 25.0) 60.0))
	      (acceleration-time 4 ;; (round (+ 1.0 (/ (+ leg-space 1.0) 10.0)))
				 ))
	 (setf vel forward-speed)
	 (setf accel-time acceleration-time)
	 (setf base-accel (+ 0.2 ;; (* leg-space 0.04)
			     ))
	 (setf max-vel (+ 2.0  ;; (* 2.0 (/ (abs forward-speed) max-entrance-speed))
			  ))
	 (setf prep-base-deccel (+ 0.3 (* base-accel 0.8)))
	 (setf deccel-max-vel (+ max-vel (- 0.3 (* 0.2 (/ max-vel (+ 1.7 1.7)) ;;Max vel's maximum.
						   ))))
	 (setf punch-time (round (- 3.0 (/ (- leg-space *neutral-leg-space*) 3))))
	 (setf prep-time 20);TO DO!!
	 (setf reco-time (+ 18 (round (* 5.0 (/ (abs forward-speed) max-entrance-speed))))) ;TO DO!!
	 (setf base-damage (round (* power (/ 100.0 max-power))))

	 (format t "~&************************~%")
	 (format t "~&InitSpeed: ~a~8T accel-time: ~a ~8T reco-time: ~a
~%base-damage: ~a ~8T~%"
		 forward-speed accel-time reco-time base-damage)
	 (format t "~&************************~%"))))
    
    (def-statemeth animate ()
      (let* ((ftime (float tpos))
	    (anim-pos
	     (/
	      (cond
	       ((equal action :accel) 1.0) ;;Change this!
	       ((equal action :prep) 1.0 ;; (* (/ ftime prep-time) prep-time)
		)
	       ((equal action :punch) (* prep-end-time (/ ftime (if (> punch-time 0.0) punch-time 1.0))))
	       ((equal action :reco) (+ (* (/ ftime reco-time) (- reco-end-time prep-end-time)) prep-end-time)))
	      60.0)))
	(set-time-position animation anim-pos)))))


(defstate "lunging-punch"
  :supers
  (single-attack-box)

  :alt-name
  "spunch"

  :slots
  ((vel)
   (foot-pos :initform *neutral-leg-space*))

  :main-action
  ((common-transitions)
   (move-forward vel)
   (incf foot-pos (abs vel))

   (cond
    ((<= tpos 10)
     (incf vel 0.0))

    ((<= tpos 18)
     (incf vel 0.07))

    ((= tpos 17)
     (incf vel 0.4))

    ((= tpos 19)
     (incf vel 0.7))

    ((<= tpos 20)
     (incf vel 0.35))

    ((<= tpos 21)
     (incf vel 0.1))

    ((<= tpos 22)
     (incf vel 3.0))

    ((<= tpos 25)
     (decf-to vel 0.0 0.1))

    ((= tpos 26)
     (decf-to vel 0.0 0.5)
     (set-hitbox
      (make-static-dist-rab
       :parent-state state
       :damage 100
       :hitstun 32
       :hitdist 70
       :blockstun 32
       :blockdist 70
       :x (* (get-direction fighter) 23.0) :Y 40.0
       :radius 10.0 :height 10.0)))

    ((<= tpos 29)
     (decf-to vel 0.0 1.4))

    ((= tpos 30)
     (remove-hitbox))

    ((= tpos 56)
     (switch-to-state idle :key-buffer key-buffer)))

   (format t "~&*LP* vel: ~a~&" vel)))

(defstate "Kpunch"
  :supers
  (single-attack-box movement-independent-animation)

  :slots
  ((move-back :initform nil))

  :funcs
  ((attack-time 18)
   (move-time-start (+ attack-time 8))
   (move-time-end (+ move-time-start 20)))
  
  :main-action
  ((common-transitions)
   (within 0 attack-time
	   (animate-forward 1.0))
   
   (within move-time-start move-time-end
	   (if move-back (animate-forward -0.7)))
   
   (if (and (< tpos move-time-start) (get-held (opposite-symbol)))
       (setf move-back t))
   (lcase tpos
	  (attack-time (set-hitbox
			(make-static-dist-rab
			 :parent-state state
			 :damage 100
			 :hitstun 32
			 :hitdist 48
			 :blockstun 32
			 :blockdist 48
			 :x (* (get-direction fighter) 16.0) :Y 38.0
			 :radius 8.0 :height 8.0)))
	  
	  ((+ attack-time 2) (remove-hitbox))

	  (30 (if (not move-back) (switch-to-state idle :key-buffer key-buffer)))

	  (move-time-end (switch-to-state idle :key-buffer key-buffer))))

  :rest
  (progn
    (def-statemeth animate ()
      (scale-animation (list 0 attack-time move-time-end) (list 0 15.0 40.0)))
    (def-statemeth height ()
      (cond
       ((< tpos 8) (* (call-next-method) 1.0))
       ((< tpos attack-time) (* (call-next-method) 0.6))
       (t (* (call-next-method) 0.6))))))


(defstate "sidekickM"
  :hb
  multi-hitbox

  :supers
  (single-attack-box partial-tracking movement-independent-animation)

  :slots
  ((hip-box))

  :funcs
  ((attack-time 32)
   (threat-end-time (+ attack-time 4))
   (end-time 58)
   (move-speed 0.9)
   (damage-scale (/ (- (* 1.0 threat-end-time) tpos) (- threat-end-time attack-time)))
   (min-attack-radius 5.0)
   (max-attack-radius 15.0))

  :entryfunc
  (let ((hip (make-uni-box state 0.0 (* 0.4 (height fighter)) 0.65 0.3)))
    (setf (hip-box state) hip)
    (setf (hitbox-list state)
	  (list
	   (make-uni-box state 0.0 0.0 1.0 0.4)
	   hip
	   (make-uni-box state 0.0 (* 0.7 (height fighter)) 0.5 0.3))))

  :main-action
  ((common-transitions)
   (if (<= tpos attack-time)
       (if (<= tpos 5)
	   (animate-forward (* 0.4 move-speed))
	 (animate-forward move-speed))
     (progn
       (animate-forward (* 0.7 move-speed))
       (when (<= tpos threat-end-time)
	 (let ((increment (/ (- max-attack-radius min-attack-radius) (- threat-end-time attack-time))))
	   (incf (radius hitbox) increment)
	   (incf (slot-value hitbox 'x) (* (get-direction fighter) increment))))))
   (within 0 28
	   (if (or (not (get-held :a2)) (get-pressed :dodge) (get-pressed :cancel) (get-pressed :defense))
	       (switch-to-state sidekickS-a
				:key-buffer key-buffer
				:entry-time (round (- 8 (/ tpos 10.0)))
				:move-speed 0.4)))
   (lcase tpos
	  (attack-time
	   (set-hitbox
	    (make-static-dist-rab 
	     :parent-state state
	     :damage (round (* 130 damage-scale))
	     :hitstun (round (* (- (+ 12 end-time) tpos) damage-scale))
	     :hit-movement-time 20
	     :hitdist (* 60 damage-scale)
	     :blockstun (round (* (- (+ 2 end-time) tpos) damage-scale))
	     :block-movement-time 16
	     :blockdist (* 45 damage-scale)
	     :x (* (get-direction fighter) 21.0) :Y 34.0
	     :radius min-attack-radius :height 8.0)))

	  (threat-end-time
	   (remove-hitbox))

	  (end-time
	   (switch-to-state idle :key-buffer key-buffer)))
   (format t "~&*~a ~a*~&" tpos damage-scale))

  :rest
  (def-statemeth linear-tracking ()
      nil))

(defstate "sidekickW"
  :hb
  multi-hitbox

  :supers
  (single-attack-box partial-tracking)

  :slots
  ((hip-box)
   (extra-time :initform 0))

  :funcs
  ((attack-time (+ 20 extra-time))
   (threat-end-time (+ attack-time 5))
   (end-time 50)
   (move-speed 0.3))

  :entryfunc
  (let ((hip (make-uni-box state 0.0 (* 0.4 (height fighter)) 0.65 0.3)))
    (setf (hip-box state) hip)
    (setf (hitbox-list state)
	  (list
	   (make-uni-box state 0.0 0.0 1.0 0.4)
	   hip
	   (make-uni-box state 0.0 (* 0.7 (height fighter)) 0.5 0.3))))

  :main-action
  ((common-transitions)
   (incf x (* move-speed (get-direction fighter)))
   (lcase tpos
	  (attack-time
	   (set-hitbox
	    (make-static-dist-rab 
	     :parent-state state
	     :damage 80
	     :hitstun (- (+ 3 end-time) tpos)
	     :hit-movement-time 25
	     :hitdist 54
	     :blockstun (- (- end-time 7) tpos)
	     :block-movement-time 20
	     :blockdist 40
	     :x (* (get-direction fighter) 31.0) :Y 34.0
	     :radius 14.0 :height 8.0)))

	  (threat-end-time
	   (remove-hitbox))

	  (end-time
	   (switch-to-state idle :key-buffer key-buffer))))

  :rest
  (progn
    (def-statemeth animate ()
      (scale-animation (list 0 attack-time end-time) (list 0 23.0 61.0))
      (if (get-enabled animation)
	  (set-position-f (ogre-node parent) (- x (float (* tpos move-speed (get-direction fighter)))) y 0.0)))

    (def-statemeth linear-tracking ()
      nil)))

(defstate "kick-feint"
  :main-action
  ((when (>= tpos 8)
     (switch-to-state idle :key-buffer key-buffer))))


(defstate "sidekickS"
  :hb
  multi-hitbox

  :supers
  (single-attack-box partial-tracking movement-independent-animation)

  :slots
  ((hip-box)
   (signaled :initform nil))

  :funcs
  ((startup-time 9)
   (attack-time 38)
   (threat-end-time (+ attack-time 4))
   (end-time 62)
   (move-speed 1.3))

  :entryfunc
  (let ((hip (make-uni-box state 0.0 (* 0.4 (height fighter)) 0.65 0.3)))
    (setf (hip-box state) hip)
    (setf (hitbox-list state)
	  (list
	   (make-uni-box state 0.0 0.0 1.0 0.4)
	   hip
	   (make-uni-box state 0.0 (* 0.7 (height fighter)) 0.5 0.3))))

  :main-action
  ((if (<= tpos threat-end-time)
       (if (<= tpos startup-time)
	   (animate-forward (* 0.4 move-speed))
	 (animate-forward move-speed))
     (animate-forward (* 0.6 move-speed)))

   (common-transitions)
   
   (within 0 11
	   (if (or (not (get-held :a2)) (get-pressed :dodge) (get-pressed :cancel)
		   ;; (get-pressed :a2)
		   ;; (get-pressed :cancel)
		   (get-pressed :defense))
	       (switch-to-state sidekickS-a
				:key-buffer key-buffer
				:entry-time tpos)))

   (within 11 23
	   (if (or (not (get-held (direction-symbol)))
		   (get-pressed :defense))
	       (setf signaled t)))
   
   (when (and (= tpos 34) signaled)
     (switch-to-state glide-in
		      :key-buffer key-buffer
		      :vel (* 0.5 move-speed)
		      :foot-pos 14.0))

   ;; (within 23 34
;; 	   (if (or (not (get-held (direction-symbol)))
;; 		   (get-pressed :defense))
;; 	       (setf signaled t)))

;;    (when (and (= tpos 34) signaled)
;;      (setf tpos (+ attack-time 10)))
   
   (lcase tpos
	  (attack-time
	   (set-hitbox
	    (make-static-dist-rab 
	     :parent-state state
	     :damage 160
	     :hitstun (- (+ 28 end-time) tpos)
	     :hit-movement-time 25
	     :hitdist 100
	     :blockstun (- (+ 22 end-time) tpos)
	     :block-movement-time 20
	     :blockdist 80
	     :x (* (get-direction fighter) 31.0) :Y 34.0
	     :radius 15.0 :height 8.0)))

	  (threat-end-time
	   (remove-hitbox))

	  (end-time
	   (switch-to-state idle :key-buffer key-buffer))))

  :rest
  (progn
    (def-statemeth animate ()
      (scale-animation (list 0 attack-time end-time) (list 0 38 65.0)))
    
    (def-statemeth linear-tracking ()
      nil)))

(defstate "sidekickS-a"

  :supers
  (movement-independent-animation)

  :hb
  multi-hitbox

  :slots
  ((entry-time);;The tpos from which the state was entered. This does not change.
   (move-speed :initform 0.4))

  :funcs
  ((end-time 23))

  :entryfunc
  (let ((hip (make-uni-box state 0.0 (* 0.4 (height fighter)) 0.65 0.3)))
    (setf (hitbox-list state)
	  (list
	   (make-uni-box state 0.0 0.0 1.0 0.4)
	   hip))
    (setf (tpos state) (entry-time state)))

  :main-action
  ((common-transitions)
   (animate-forward (if (< tpos 12) move-speed (* 0.3 move-speed)))
   (lcase tpos
	  (end-time
	   (switch-to-state idle :key-buffer key-buffer)))))


(defstate "frontkick"
  :hb
  multi-hitbox
  
  :supers
  (single-attack-box partial-tracking)

  :slots
  ((vel)
   (foot-pos)
   (attack-time :initform nil))

  :funcs
  ((kick-start-point 14) ;Earliest start of kick.
   (kick-point (+ kick-start-point 6))) ;Earliest attack-time.

  :main-action
  ((common-transitions)
   (if (not (< (abs vel) 0.15))
       (let ((dash-accel (* (signum vel) (hs-accel vel foot-pos))))
	 (decf vel dash-accel)
	 (decf foot-pos (abs vel))
	 (move-forward vel))
     (setf vel 0.0))

   (cond
    ((and (not attack-time)
	  (>= tpos kick-point)
	  (< (abs vel) 0.15))
     (setf attack-time tpos)
     (set-hitbox (make-static-dist-rab 
		  :parent-state state
		  :damage 60
		  :hitstun (- (+ 40 attack-time 2) tpos)
		  :hit-movement-time 10
		  :hitdist 50
		  :blockstun (- (+ 40 attack-time) 10 tpos)
		  :block-movement-time 10
		  :blockdist 45
		  :x (* (get-direction fighter) 18.0) :Y 30.0
		  :radius 10.0 :height 10.0)))

    ((when attack-time (between attack-time (+ 5 attack-time)))
     (incf (slot-value hitbox 'x)  (* 3.0 (get-direction fighter))))

    ((and attack-time (= tpos (+ 16 attack-time))) (remove-hitbox))
    
    ((and attack-time (= tpos (+ 40 attack-time))) (switch-to-state idle :key-buffer key-buffer))))

  :rest
  (def-statemeth linear-tracking ()
      nil))

(defmethod enter-state :after ((f1 fighter) (state frontkick))
  (setf (hitbox-list state)
	(list
	 (make-instance 'uni-box
			:parent state
			:x 0.0 :y 0.0
			:radius 1.0 :height 1.0))))

(defstate "bodykick"
  :hb
  multi-hitbox

  :alt-name
  "bodykick-w"

  :slots
  ((hip-box)
   (end-time :initform 36))

  :entryfunc
  (let ((hip (make-uni-box state 0.0 (* 0.4 (height fighter)) 0.65 0.3)))
    (setf (hip-box state) hip)
    (setf (hitbox-list state)
	  (list
	   (make-uni-box state 0.0 0.0 1.0 0.4)
	   hip
	   (make-uni-box state 0.0 (* 0.7 (height fighter)) 0.5 0.3))))
  
  :supers
  (single-attack-box)

  :funcs
  ((attack-time 13))

  :main-action
  ((common-transitions)
   (when (<= 11 tpos attack-time)
     (incf (slot-value hip-box 'radius) 0.15))
   (when (<= (+ 6 attack-time) tpos (+ 16 attack-time))
    (decf (slot-value hip-box 'radius) 0.15))
   
   (lcase tpos
	  (attack-time
	   (set-hitbox
	    (make-static-dist-rab 
	     :parent-state state
	     :damage 60
	     :hitstun (- end-time tpos)
	     :hitdist 25
	     :blockstun (- end-time 12 tpos)
	     :blockdist 30
	     :x (* (get-direction fighter) 26.0) :Y 34.0
	     :radius 12.0 :height 6.0)))
	  
	  ((+ attack-time 1)
	   (remove-hitbox))
	  
	  (end-time
	   (switch-to-state idle :key-buffer key-buffer))))

  :rest
  (progn
    (def-statemeth animate ()
      (scale-animation (list 0 attack-time end-time) (list 0 14.0 42.0)))
    
    (def-statemeth attack-blocked (other-state)
      (ccnm)
      (setf end-time 46))))



(defstate "spinning-hook-kick"
  :supers
  (single-attack-box)

  :hb
  multi-hitbox

  :constants
  ((attack-point 23)
   (end-point 53))

  :slots
  ((hip-box :initform nil))

  :entryfunc
  (progn
    (let ((hip (make-uni-box state 0.0 (* 0.4 (height fighter)) 0.65 0.3)))
     (setf (hip-box state) hip)
     (setf (hitbox-list state)
	   (list
	    (make-uni-box state 0.0 0.0 1.0 0.4)
	    hip))))

  :main-action
  ((common-transitions)
   (when (<= tpos attack-point)
     (setf (x hip-box)  (* 0.75 (get-direction) (/ tpos (float attack-point))))
     (setf (radius hip-box) (+ 0.65 (* 0.75 (/ tpos (float attack-point))))))
   (lcase tpos
	  (attack-point (set-hitbox
			 (make-static-dist-rab
			  ;;:class 'unblockable-box
			  :parent-state state
			  :damage 140
			  :hitstun 30
			  :hitdist 70
			  :blockstun 30
			  :blockdist 80
			  :x (* (get-direction fighter) 30.0) :Y 52.0
			  :radius 9.0 :height 9.0)))
	  
	  ((+ 1 attack-point) (remove-hitbox))
	  
	  (end-point (switch-to-state idle :key-buffer key-buffer))))

  :rest
  (progn
    (def-statemeth animate ()
      (scale-animation (list 0 13 attack-point end-point) (list 0 20.0 28.0 60.0)))))

(defstate "forward-hook-kick"
  :supers
  (single-attack-box movement-independent-animation)

  :funcs
  ((attack-time 25)
   (end-time 58))

  :main-action
  ((common-transitions)
   (within 0 5
	   (animate-forward 0.6))
   (within 5 12
	   (animate-forward 1.2))

   (lcase tpos
	  (attack-time
	   (set-hitbox
	    (make-static-dist-rab
	     ;;:class 'unblockable-box
	     :parent-state state
	     :damage 140
	     :hitstun 40
	     :hitdist 50
	     :blockstun 40
	     :blockdist 60
	     :x (* (get-direction fighter) 27.0) :Y 52.0
	     :radius 10.0 :height 9.0)))
	  ((+ 2 attack-time) (remove-hitbox))
	  (end-time
	   (switch-to-state idle :key-buffer key-buffer))))

  :rest
  (progn
    (def-statemeth animate ()
      (scale-animation (list 0 attack-time end-time) (list 0 30.0 60.0)))))

(defstate floating-haymaker
  :supers
  (single-attack-box)

  :funcs
  ((attack-time 25)
   (end-time 40)))


(defstate rising-haymaker
  :supers
  (single-attack-box)

  :alt-name
  "jab"

   :hb
  multi-hitbox

  :funcs
  ((attack-time 25)
   (end-time 40))

  :slots
  ((hip-box))

  :entryfunc
  (let ((hip (make-uni-box state 0.0 (* 0.4 (height fighter)) 0.65 0.3)))
    (setf (hip-box state) hip)
    (setf (hitbox-list state)
	  (list
	   (make-uni-box state 0.0 0.0 1.0 0.4)
	   hip
	   (make-uni-box state 0.0 (* 0.7 (height fighter)) 0.5 0.3))))

  :main-action
  ((common-transitions)
   (lcase tpos
	  (attack-time (set-hitbox
			(make-static-dist-rab
			 :parent-state state
			 :damage 85
			 :hitstun 40
			 :hitdist 50
			 :blockstun 40
			 :blockdist 40
			 :x (* (get-direction fighter) 24.0) :Y 48.0
			 :radius 6.0 :height 8.0)))
	  
	  ((+ 2 attack-time) (remove-hitbox))
	  
	  (end-time (switch-to-state idle :key-buffer key-buffer)))))