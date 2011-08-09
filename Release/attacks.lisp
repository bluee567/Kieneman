(in-package "KIENEMAN")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; attached-box
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Mixin
(defclass+ attached-box ()
  ((:iea parent)
  (hit-objects
    :initform nil
    :accessor hit-objects)))

;;This method prevents attached hitboxes from colliding with the
;;target box(es) of their own parent character by adding the parent to their
;;list of hit objects.
(defmethod initialize-instance :after ((box attached-box) &key)
  (with-accessors ((parent-state parent)) box
    (with-accessors((parent parent)) parent-state
     (push parent (hit-objects box)))))


(defmethod x ((box attached-box))
  (+ (slot-value box 'x) (x (parent (parent box)))))

(defmethod y ((box attached-box))
  (+ (slot-value box 'y) (y (parent (parent box)))))



(defun attached-box-v-fighter (ab fighter)
  (let ((state (parent ab)))
    (if (linear-tracking state)
	t
      (< (side-dist state) (radius fighter)))))

;;Makes sure that attached boxes posessing the same parent don't collide with each other.
(defmethod collision :around ((hb1 attached-box) (hb2 attached-box))
  (and (not (eql (parent hb1) (parent hb2))) (call-next-method)))

;;For the sake of accounting for linear tracking.
(defmethod collision :around ((ahb attached-box) (hb2 fighter))
  (and (attached-box-v-fighter ahb hb2) (call-next-method)))

(defmethod collision :around ((hb2 fighter) (ahb attached-box))
  (and (attached-box-v-fighter ahb hb2) (call-next-method)))
  
  
(defclass+ clash-tribox (attached-box displayed-tribox mortal)
	())

(defmethod animate ((anim clash-tribox))
  t)

(defmethod material-name ((obj clash-tribox))
	"green")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rec-attack-box
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass+ attack-box (attached-box mortal)
	())
	 
(defmethod main-action ((rab attack-box))
  t)

(defmethod animate ((obj attack-box))
  t)

(defmethod material-name ((obj attack-box))
	"red")

(defmethod-duel handle-collision ((rab attack-box) (fighter fighter))
  (when (= 0 (loop for e in (hit-objects rab) count (eq e fighter)))
    ;;When this hitbox has not previously hit the fighter.
    (push fighter (hit-objects rab)) ;Add fighter to list of hit objects.
    (handle-collision rab (state fighter))))

;(defmethod handle-collision ((fighter fighter) (rab attack-box))
;  (handle-collision rab fighter))

(defclass+ rec-attack-box (attack-box default-hitbox)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rec-strike-box
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass+ strike-box (attack-box)
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
(defmethod handle-collision ((rab strike-box) (state state))
  (with-accessors ((fighter parent)) state
   (decf (hp fighter) (damage rab))		;Hurt him!
   (change-state fighter (make-hitstun rab fighter))))

#|(defmethod handle-collision ((rab strike-box) (state high-block))
     (with-accessors ((fighter parent)) state		
       (change-state fighter (make-block-stun rab fighter))
       (attack-blocked (parent rab) state)))
	   |#
(defclass+ rec-strike-box (strike-box default-hitbox) ())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Leaves the defender a set distance away from the attacker on a hit.
(defclass+ attack-sdist-box (strike-box)
  ((:iea hitdist)
   (:ia hit-movement-time)
   (:iea min-hitdist)
   (:iea blockdist)
   (:ia block-movement-time)
   (:iea min-blockdist)))

(defclass+ rec-attack-sdist-box (attack-sdist-box default-hitbox) ())

(defclass+ tri-attack-sdist-box (attack-sdist-box displayed-tribox) ())

(defclass+ unblockable-box (rec-attack-sdist-box)
  ())

(defstate "grab"
  :supers
  (single-attack-box)

  :slots
  ((grabbed-entity :initform nil)
   (throw-state :initform nil))

  :main-action
  ((cond

    ((not throw-state)
     (cond
      ((= tpos 3)
       (set-hitbox
	(make-instance 'grab-hitbox
		       :parent state
		       :x (* (+ 2.0 radius) (get-direction fighter)) :y 30.0
		       :radius 2.0 :height 25.0)))

      ((and (> tpos 3) (not grabbed-entity))
       (when (not (get-held :a1))
	    (set-tension :outside-stance 10)
		(switch-to-state 'idle)))

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
					:parent grabbed-entity
				     :duration 15
				     :kb-direction (get-direction parent)
				     :kb-speed 3.0
				     :decceleration 0.25))
       (switch-to-state 'high-block-stun
			:duration 14
			:kb-direction (opposite (get-direction parent))
			:kb-speed 0.0
			:decceleration 0.0
			:key-buffer key-buffer)))))))

(defclass+ grab-hitbox (rec-attack-box)
  ())

(defun grabbing (rab state)
  (with-accessors ((submitter parent)) state	
    (with-accessors ((parent-state parent)) rab
      (setf (grabbed-entity parent-state) submitter)
      (change-state submitter (make-instance 'grabbed
						:parent submitter
					     :grabber parent-state)))))

(defun clinch-break (ent1 ent2)
  (change-state ent1 (make-instance 'stunned
				    :duration 10
					:parent ent1
				    :kb-direction (opposite (get-direction ent2))
				    :kb-speed 3.0
				    :decceleration 0.25))
  (change-state ent2 (make-instance 'stunned
				    :duration 10
					:parent ent2
				    :kb-direction (get-direction ent1)
				    :kb-speed 3.0
				    :decceleration 0.25)))

(defmethod handle-collision ((rab grab-hitbox) (state state))
  (grabbing rab state))

(defmethod handle-collision ((rab grab-hitbox) (state grab))
  (with-accessors ((parent-state parent)) rab		
    (let ((hitbox-entity (parent parent-state))
	  (grabbed-entity (grabbed-entity state))
	  (other-entity (parent state)))
      (if (eql grabbed-entity hitbox-entity) ;When two entities grab each other on the same frame.
	  (clinch-break hitbox-entity other-entity)
	(grabbing rab state)))))


(defun make-static-dist-rab (&key
			     (class 'rec-attack-sdist-box)
			     parent
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
		  :parent parent
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

		  
		 ;;HORRIBLE CODE DUPLICATION HERE, REFACTOR.
(defun make-tri-static-dist-rab (&key
			     (class 'tri-attack-sdist-box)
			     parent
			     damage
			     hitstun
			     hit-movement-time
			     hitdist
			     (min-hitdist 0)
			     blockstun
			     block-movement-time
			     blockdist
			     (min-blockdist 0)
			     scalar-list
				 point-list
				 x y)
  (let* ((hit-deccel (/ (* 2 (float hitdist)) (* hitstun hitstun)))
	 (hit-init-vel (* hit-deccel hitstun))
	 (block-deccel (/ (* 2 (float blockdist)) (* blockstun blockstun)))
	 (block-init-vel (* block-deccel blockstun)))
    (if (not hit-movement-time) (setf hit-movement-time hitstun))
    (if (not block-movement-time) (setf block-movement-time hitstun))
   (make-instance class
		  :parent parent
		  :damage damage
		  :hitstun hitstun
		  :hit-movement-time hit-movement-time
		  :hitdist hitdist
		  :min-hitdist min-hitdist
		  :blockstun blockstun
		  :block-movement-time block-movement-time
		  :blockdist blockdist
		  :min-blockdist min-blockdist
		  :x x :y y
		  :scalar-list scalar-list
		  :point-list point-list)))


(defun make-hitstun (rab fighter)
  (make-instance 'stunned
		 :duration (hitstun rab)
		 :parent fighter
		 :kb-direction (get-direction (parent (parent rab)))
		 :kb-speed (hitspeed rab)
		 :decceleration (hitdeccel rab)))

(defun hit-collision (rab state)
  (with-accessors ((fighter parent)) state
		  (with-accessors ((attack-state parent) (hitdist hitdist)
				   (hitstun hitstun) (hitdeccel hitdeccel) (hit-movement-time hit-movement-time)
				   (hitspeed hitspeed) (min-hitdist min-hitdist)) rab
				   (with-accessors ((attacker parent)) attack-state
						   (decf (hp fighter) (damage rab))
						   (let* ((actual-dist (- hitdist (ground-dist fighter attacker)))
							  (actual-dist (if (> actual-dist min-hitdist)
									   actual-dist min-hitdist)))
						     (setf hitdeccel (/ (* 2 (float actual-dist)) (* hit-movement-time hit-movement-time)))
						     (setf hitspeed (* hitdeccel hit-movement-time))
						     (change-state fighter (make-hitstun rab fighter)))))))

(defmethod handle-collision ((rab attack-sdist-box) (state state))
  (hit-collision rab state))

(defun make-block-stun (rab fighter)
  (block-collision-prep rab (state fighter))
  (make-instance 'high-block-stun
		    :duration (blockstun rab)
			:parent fighter
		    :kb-direction (get-direction (parent (parent rab)))
		    :kb-speed (blockspeed rab)
		    :decceleration (blockdeccel rab)))

;;Prepared the attack box for make-block-stun. 
(defmethod block-collision-prep (rab state)
	t)

(defmethod block-collision-prep ((rab attack-sdist-box) state)
  (with-accessors
   ((fighter parent)) state
   (with-accessors
    ((attack-state parent) (blockdist blockdist)
     (blockstun blockstun) (blockdeccel blockdeccel) (block-movement-time block-movement-time)
     (blockspeed blockspeed) (min-blockdist min-blockdist)) rab
     (with-accessors
      ((attacker parent)) attack-state
	  
      (let* ((actual-dist (- blockdist (ground-dist fighter attacker)))
			(actual-dist (if (> actual-dist min-blockdist)
			      actual-dist min-blockdist)))
			(setf blockdeccel (/ (* 2 (float actual-dist)) (* block-movement-time block-movement-time)))
			(setf blockspeed (* blockdeccel block-movement-time)))))))

#|(defmethod handle-collision ((rab attack-sdist-box) (state high-block))
  (with-accessors
   ((fighter parent)) state
   (with-accessors
    ((attack-state parent) (blockdist blockdist)
     (blockstun blockstun) (blockdeccel blockdeccel) (block-movement-time block-movement-time)
     (blockspeed blockspeed) (min-blockdist min-blockdist)) rab
     (with-accessors
      ((attacker parent)) attack-state
	  
      (let* ((actual-dist (- blockdist (ground-dist fighter attacker)))
	     (actual-dist (if (> actual-dist min-blockdist)
			      actual-dist min-blockdist)))
			(setf blockdeccel (/ (* 2 (float actual-dist)) (* block-movement-time block-movement-time)))
			(setf blockspeed (* blockdeccel block-movement-time))
			(change-state fighter (make-block-stun rab fighter))
			(attack-blocked (parent rab) state))))))
	|#

(defmethod handle-collision ((rab unblockable-box) (state high-block))
  (hit-collision rab state))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstate "jab"

  :supers
  (clash-attack-box)
  
  :constants
  ((end-movement 15))

  :slots
  ((tpos-delay :initform 0) ;Used to delay tpos when canceling the attack.
   (cancel-held-at-start :initform nil)
   (forward-speed :initform 0)
   (to-second :initform nil)
   (move-speed :initform 0.0)
   (tip-point :initform nil))

  :animation
  single-animation
  
  :initfunc
  ((&key) (setf tip-point (make-instance 'child-vector :x 15.0 :y 56.0 :parent state)))

  :main-action
  ((when (and (> tpos-delay 0) (>= tpos 5))
     (setf tpos-delay 0)
     (setf tpos 14));Must eval to an integer to avoid type errors.

   (if (and (get-held :cancel) (< tpos 4))
       (setf tpos-delay 1))
	   
     (progn
	 ;if (and nil (get-pressed :a1) (not to-second))
	 ;(setf to-second t)
	 (common-transitions))
	 
   ;bink
   (case tpos
	 (4 (set-clashbox (make-instance 'clash-tribox
				   :parent state
				   :x (* (get-direction fighter) 5.0) :Y 50.0
				   :scalar-list (list 0.0 5.0  0.0 0.0  7.0 5.0))))
     (6 (set-hitbox (make-tri-static-dist-rab
				   :parent state
				   :damage 50
				   :hitstun 18
				   :hit-movement-time 14
				   :hitdist 38
				   :min-hitdist 5
				   :blockstun 8
				   :block-movement-time 6
				   :blockdist 30
				   :x (* (get-direction fighter) 20.0) :Y 53.0
				   :point-list (list (make-instance 'child-vector :x 12.0 :y 56.0 :parent state) (make-instance 'child-vector :x 12.0 :y 52.0 :parent state) tip-point))
				   #|(make-static-dist-rab
				   :parent state
				   :damage 50
				   :hitstun 18
				   :hit-movement-time 14
				   :hitdist 38
				   :min-hitdist 5
				   :blockstun 8
				   :block-movement-time 6
				   :blockdist 30
				   :x (* (get-direction fighter) 20.0) :Y 52.0
				   :radius 5.0 :height 5.0)|#))
     (10 (remove-hitbox))
     (25 (set-tension :front-pressure 10) (switch-to-state 'idle :key-buffer key-buffer)))
	 
	 (when (<= 7 tpos 9) (setf (x tip-point) (+ (slot-value tip-point 'x) 5.0)))
	 
	 (when (<= tpos end-movement)
		(move-forward move-speed))
   
   (within 12 25
	   (when to-second
	     (switch-to-state 'second-jab))))
	
	:tensions-resolved
	(not (or (tensions-exist :exceptions '(:rear-pressure)) (get-minor-tension :lead-hand-use)))
  
  :rest
  (progn
   (defclass+ jab-box (rec-attack-sdist-box)
     ())
	 
	 (def-statemeth mutual-clash :after ((other jab))
		(when (< tpos end-movement)
		  (let*
			((actual-dist (- 30.0 (ground-dist parent (parent other))))
			(actual-speed (min 0.0 (/ actual-dist (- tpos end-movement)))))
				(setf move-speed actual-speed)
				(setf (move-speed other) actual-speed))))
	
	 (def-statemeth attack-blocked (other-state)
		(ccnm)
		(when (< tpos end-movement)
		  (let*
			((actual-dist (- 25.0 (ground-dist parent (target parent))))
			(actual-speed (min 0.0 (/ actual-dist (- tpos end-movement)))))
				(setf move-speed actual-speed))))
  
   (defmethod handle-collision ((rab jab-box) (state idle))
     (with-accessors ((fighter parent)) state		
		     (change-state fighter (make-block-stun rab fighter))))

   (defmethod animate ((sa jab))
     (let ((tp (/ (float (tpos sa)) 60.0))
	   (ani (animation sa)))
       (set-time-position ani tp)))))


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
  ((common-transitions)
  (lcase tpos
	  (attack-time (set-hitbox (make-static-dist-rab
			   :parent state
			   :damage 70
			   :hitstun 15
			   :hitdist 45
			   :blockstun 5
			   :blockdist 40
			   :x (* (get-direction fighter) 24.0) :Y 50.0
			   :radius 5.0 :height 5.0)))
	  
	  ((+ attack-time 2) (remove-hitbox))
	  
	  (end-time (switch-to-state 'idle :key-buffer key-buffer))))
  
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
(defconstant cancel-extra 16)

(defstate "roundhouse"

  :hb
  multi-hitbox
  
  :supers
  (clash-attack-box)
  
  :slots
  ((forward-speed :initform 0.0)
   (forward-dist :initform 0.0)
   (hip-box :initform nil)
   (blocked :initform nil)
   (cancel-time :initform nil))

  :funcs
  ((clash-time 13)
   (attack-time 20)
   (block-time (+ 40 (if blocked 12 0)))
   (end-time (+ block-time 12)))

  :animation
  single-animation

  :initfunc
  ((&key)
    (let ((hip (make-uni-box state 0.0 (* 0.4 (height fighter)) 0.65 0.3)))
     (setf (hip-box state) hip)
     (setf (hitbox-list state)
	   (list
	    (make-uni-box state 0.0 0.0 1.0 0.4)
	    hip
	    (make-uni-box state 0.0 (* 0.7 (height fighter)) 0.5 0.3)))))

  :main-action
  ((common-transitions)
    
	(when (> forward-dist 0.0)
		(if (> forward-dist forward-speed)
			(progn (move-forward forward-speed) (decf forward-dist forward-speed))
		 (progn (move-forward forward-dist) (setf forward-dist 0.0))))
  
	(when (and (< tpos clash-time) (eql forward-speed 0.0) (get-pressed :cancel))
		(setf cancel-time tpos))

   (when (and (>= tpos block-time) (equal 'high-block (class-name (class-of (get-buffered-state)))))
	 ;;(use-buffered-state)
	 (set-buffered-state nil)
     (switch-to-state 'high-block
		      :block-startup (+ (- end-time tpos) 10)))

   (when (<= tpos attack-time)
    (setf (radius hip-box) (+ 0.65 (* 1.5 (/ tpos (float attack-time))))))
   
   (if (not cancel-time)
   (lcase tpos
		(clash-time (set-clashbox (make-instance 'clash-tribox
				   :parent state
				   :x (* (get-direction fighter) 24.0) :Y 32.0
				   :scalar-list (list -10.0 8.0  -10.0 0.0  7.0 20.0))))
		(attack-time (set-hitbox
			(make-static-dist-rab
			 :parent state
			 :damage 90
			 :hitstun 20
			 :hitdist 48
			 :blockstun 8
			 :blockdist 40
			 :x (* (get-direction fighter) 36.0) :Y 52.0
			 :radius 9.0 :height 8.0)))
     
		((+ 1 attack-time) (remove-hitbox))
	 
		(end-time (set-tension :rear-leg-neutral 10) (switch-to-state 'idle :key-buffer key-buffer)))
	 ;;If cancel-time
	 (if (= tpos (+ cancel-extra (* 2 cancel-time))) (switch-to-state 'idle :key-buffer key-buffer))))

	;:tensions-resolved
	;(not (tensions-exist :exceptions '(:kick-recovery)))

  :rest
  (progn
    (def-statemeth animate ()
	(let ((atime ))
		(scale-animation (list 0.0 14.0 attack-time end-time) (list 0.0 5.0 12.0 26.0)
		:time (if cancel-time (if (< (- tpos cancel-time) (/ cancel-extra 2)) cancel-time (max 1 (- (* 2 cancel-time) tpos))) tpos))
		(format t "~&tpos: ~a c-t: ~a~&" tpos cancel-time)))

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
  (clash-attack-box)

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
   (end-movement)
   (base-damage)
   (max-vel)
   (deccel-max-vel)
   (prep-base-deccel)
   ;;These values are 'live' and modifiable.
   (vel))

  :animation
  single-animation

  :main-action
  ((let* ((dir (signum vel)))
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
	   (setf vel 0.0)
	   ;;Resets the timer to 0, allowing the hitbox to be destroyed with precision.
	   (setf tpos 0)))))

     (:punch
      (if (<= tpos punch-time)
	  ()
	(progn
	  ;;Sets the hitbox.
	  (set-clashbox (make-instance 'clash-tribox
				   :parent state
				   :x (* (get-direction fighter) 10.0) :Y 42.0
				   :scalar-list (list -4.0 8.0  -4.0 0.0  4.0 4.0)))
	  (set-hitbox
	    (make-instance 'rec-strike-box
			   :parent state
			   :damage base-damage
			   :hitstun (+ 2 reco-time)
			   :hitspeed 1.6
			   :hitdeccel 0.08
			   :blockstun (- reco-time 10)
			   :blockspeed 1.0
			   :blockdeccel 0.08
			   :x (* (get-direction fighter) 22.0) :Y 42.0
			   :radius 7.0 :height 6.0))
	  (setf action :reco)
	  (setf tpos 0))))
    
     (:reco
      ;;After the attack box has already been deployed.
      (when (= tpos 2)
		(remove-hitbox))
	  (when (<= tpos end-movement)
		(move-forward vel))
      (when (> tpos reco-time)
		(set-tension :front-pressure 10)
		(switch-to-state 'idle :key-buffer key-buffer))))
	(common-transitions)
    (format t "Action: ~a ~10T Tpos: ~a ~10T Vel: ~a" action tpos vel)
    (format t "~&************************~%")))
  
  
  :rest
  (progn
    (defmethod initialize-instance :after ((state straight) &key)
      (with-accessors ((forward-speed forward-speed) (accel-time accel-time) (prep-time prep-time) (reco-time reco-time) (punch-time punch-time) (base-damage base-damage)
		       (base-accel base-accel) (prep-base-deccel prep-base-deccel) (max-vel max-vel) (deccel-max-vel deccel-max-vel) (leg-space leg-space) (vel vel) (end-movement end-movement)) state
       (let* ((power (+ forward-speed extra-speed))
	      (total-accel (/ (+ leg-space 25.0) 60.0)))
	 (setf vel forward-speed)
	 (setf accel-time 10)
	 (setf base-accel 0.2)
	 (setf max-vel 2.0)
	 (setf prep-base-deccel 0.4)
	 (setf deccel-max-vel (+ max-vel (- 0.3 (* 0.2 (/ max-vel 3.4))))) ;;Max vel's maximum.
	 (setf punch-time (round (- 3.0 (/ (- leg-space *neutral-leg-space*) 3))))
	 (setf prep-time 20);TO DO!!
	 (setf reco-time (+ 18 (round (* 5.0 (/ (abs forward-speed) max-entrance-speed))))) ;TO DO!!
	 (setf end-movement (round (/ reco-time 2.0)))
	 (setf base-damage (round (* power (/ 100.0 max-power))))

	 (format t "~&************************~%")
	 (format t "~&InitSpeed: ~a~8T accel-time: ~a ~8T reco-time: ~a
~%base-damage: ~a ~8T~%"
		 forward-speed accel-time reco-time base-damage)
	 (format t "~&************************~%"))))
	 
	  (def-statemeth attack-blocked (other-state)
		(ccnm)
		(let
		((actual-dist (- 24.0 (ground-dist parent (target parent)))))
			(setf vel (/ actual-dist (- tpos end-movement)))))
    
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
  ((move-forward vel)
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
       :parent state
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
     (switch-to-state 'idle :key-buffer key-buffer)))

   (format t "~&*LP* vel: ~a~&" vel)))


(defstate "sidekickW"
  :hb
  multi-hitbox

  :supers
  (clash-attack-box movement-independent-animation partial-tracking)

  :slots
  ((hip-box)
   (leg-overstep :initform 0.0)
   (move-speed :initform 0.5)
   (move-deccel :initform 0.0))

  :funcs
  ((attack-time 22)
   (threat-end-time (+ attack-time 4))
   (end-movement 40)
   (end-time 64))

  :initfunc
  ((&key)
  (let ((hip (make-uni-box state 0.0 (* 0.4 (height fighter)) 0.65 0.3)))
    (setf (hip-box state) hip)
    (setf (hitbox-list state)
	  (list
	   (make-uni-box state 0.0 0.0 1.0 0.4)
	   hip
	   (make-uni-box state 0.0 (* 0.7 (height fighter)) 0.5 0.3)))))

  :main-action
  ((common-transitions)
  
  (when (<= tpos end-movement)
   (if (> move-speed 0.0)
	(animate-forward move-speed)
	(progn (move-forward move-speed) (move-animation move-speed))))
	
   (lcase tpos
	  (10 (set-clashbox (make-instance 'clash-tribox
				   :parent state
				   :x (* (get-direction fighter) 3.0) :Y 30.0
				   :scalar-list (list 0.0 10.0  0.0 0.0  20.0 8.0))))
	  (attack-time
	   (set-hitbox
	    (make-static-dist-rab 
	     :parent state
	     :damage 100
	     :hitstun (- (+ end-time 10) tpos)
	     :hit-movement-time 25
	     :hitdist 70
	     :blockstun (- end-time 20 tpos)
	     :block-movement-time 20
	     :blockdist 0.0
	     :x (* (get-direction fighter) 30.0) :Y 32.0
	     :radius 10.0 :height 8.0)))
		 
		((+ 1 attack-time) (setf (damage (hitbox state)) 0)
							(setf (hitdist (hitbox state)) 55)
							(setf (hitstun (hitbox state)) (- end-time 5 tpos)))

	  (threat-end-time
	   (remove-hitbox))

	  (end-time
	   (set-tension :front-leg-forward 10)
	   (switch-to-state 'idle :key-buffer key-buffer))
	   
	   (t ))
	   
	   (when (and (< (+ 5 end-movement) tpos end-time) (or (equal (type-of (get-buffered-state)) 'sidekickW) (equal (type-of (get-buffered-state)) 'continued-step)))
				(use-buffered-state)))
	
	:tensions-resolved
	(not (tensions-exist :exceptions '(:front-leg-forward :kick-recovery)))

  :rest
  (progn
	  (def-statemeth mutual-clash :after ((other sidekickW))
		(when (< tpos end-movement)
		  (let*
			((actual-dist (- 35.0 (ground-dist parent (parent other))))
			(actual-speed (min move-speed (/ actual-dist (- tpos end-movement)))))
				(setf move-speed actual-speed)
				(setf (move-speed other) actual-speed))))
		
	  (def-statemeth attack-blocked (other-state)
		(ccnm)
		(let
		((actual-dist (- 30.0 (ground-dist parent (target parent)))))
			(setf move-speed (/ actual-dist (- tpos end-movement)))))

    (def-statemeth linear-tracking ()
      nil)))


(defstate "sidekickS"
  :hb
  multi-hitbox

  :supers
  (clash-attack-box partial-tracking movement-independent-animation)

  :slots
  ((hip-box)
   ;; (animation-distance :initform 0.0)
   )

  :funcs
  ((attack-time 38)
   (threat-end-time (+ attack-time 2))
   (end-time 64)
   (move-speed 1.2))

  :initfunc
  ((&key)
	(let ((hip (make-uni-box state 0.0 (* 0.4 (height fighter)) 0.65 0.3)))
    (setf (hip-box state) hip)
    (setf (hitbox-list state)
	  (list
	   (make-uni-box state 0.0 0.0 1.0 0.4)
	   hip
	   (make-uni-box state 0.0 (* 0.7 (height fighter)) 0.5 0.3)))))

  :main-action
  ((common-transitions)
  (labels (;; (animate-forward (dist)
;; 			     (let ((dist-vec (* dist (get-direction fighter))))
;; 			      (incf x dist-vec)
;; 			      (incf animation-distance dist-vec)))
	    )
    (if (<= tpos threat-end-time)
	(animate-forward move-speed)
      (animate-forward (* 0.6 move-speed))))
   (within 0 11
	   (if (or (not (get-held :a2)) (get-pressed :cancel) (get-pressed :defense))
	       (switch-to-state 'sidekickS-a
				:key-buffer key-buffer
				:entry-time tpos)))
   (lcase tpos
	  (attack-time
	   (set-hitbox
	    (make-static-dist-rab 
	     :parent state
	     :damage 160
	     :hitstun (- (+ 28 end-time) tpos)
	     :hit-movement-time 25
	     :hitdist 100
	     :blockstun (- (+ 22 end-time) tpos)
	     :block-movement-time 20
	     :blockdist 80
	     :x (* (get-direction fighter) 35.0) :Y 34.0
	     :radius 10.0 :height 8.0)))

	  (threat-end-time
	   (remove-hitbox))

	  (end-time
	   (switch-to-state 'idle :key-buffer key-buffer))))
	   
	:tensions-resolved
	(not (tensions-exist :exceptions '(:front-leg-forward :rear-pressure :front-pressure)))

  :rest
  (progn
    ;; (def-statemeth animate ()
;;       (ccnm)
;;       (set-position-f (ogre-node parent) (- x animation-distance) y 0.0))
    (def-statemeth linear-tracking ()
      nil)))

(defstate "sidekickS-a"

  :supers
  (movement-independent-animation)

  :hb
  multi-hitbox

  :slots
  ((entry-time);;The tpos from which the state was entered. This does not change.
  (next-state :initform (list nil 0)))

  :funcs
  ((end-time 23)
   (move-speed 1.2))

  :initfunc
  ((&key)
	(let ((hip (make-uni-box state 0.0 (* 0.4 (height fighter)) 0.65 0.3)))
    (setf (hitbox-list state)
	  (list
	   (make-uni-box state 0.0 0.0 1.0 0.4)
	   hip
	   (make-uni-box state 0.0 (* 0.7 (height fighter)) 0.5 0.3)))
    (setf (tpos state) (entry-time state))))

  :main-action
  ((if (and (not (and (>= (get-axis-dist) *trigger-radius*) (equal held-dir (opposite (get-direction)))))  (get-pressed :move))
	 (setf next-state (list 'continued-step 5.0))
   (common-transitions))
   
   (animate-forward (if (< tpos 12) move-speed (* 0.3 move-speed)))
   
   (if (> tpos 20)
	(when (first next-state)
		(set-tension :front-leg-forward 20)
		(switch-to-state 'continued-step :dir positive :total-dist (second next-state))))
		
   (lcase tpos
	  (end-time
	   (set-tension :front-leg-forward 8)
	   (switch-to-state 'idle :key-buffer key-buffer))))
	
	:tensions-resolved
	(not (tensions-exist :exceptions '(:front-leg-forward))))


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
		  :parent state
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
    
    ((and attack-time (= tpos (+ 40 attack-time))) (switch-to-state 'idle :key-buffer key-buffer))))

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

  :slots
  ((hip-box)
   (end-time :initform 55))

  :initfunc
  ((&key)
  (let ((hip (make-uni-box state 0.0 (* 0.4 (height fighter)) 0.65 0.3)))
    (setf (hip-box state) hip)
    (setf (hitbox-list state)
	  (list
	   (make-uni-box state 0.0 0.0 1.0 0.4)
	   hip
	   (make-uni-box state 0.0 (* 0.7 (height fighter)) 0.5 0.3)))))
  
  :supers
  (clash-attack-box)

  :funcs
  ((attack-time 22))

  :main-action
  ((common-transitions)
  (when (<= 11 tpos attack-time)
    (incf (slot-value hip-box 'radius) 0.15))
   (when (<= (+ 6 attack-time) tpos (+ 16 attack-time))
    (decf (slot-value hip-box 'radius) 0.15))
   
   (lcase tpos
	  ((- attack-time 2)
		(set-clashbox (make-instance 'clash-tribox
				   :parent state
				   :x (* (get-direction fighter) 12.0) :Y 34.0
				   :scalar-list (list 0.0 4.0  0.0 0.0  19.0 6.0))))
	  (attack-time
	   (set-hitbox
	    (make-static-dist-rab 
	     :parent state
	     :damage 50
	     :hitstun (- end-time tpos)
	     :hitdist 25
	     :blockstun (- end-time 12 tpos)
	     :blockdist 30
	     :x (* (get-direction fighter) 40.0) :Y 34.0
	     :radius 9.0 :height 6.0)))
	  
	  ((+ attack-time 1)
	   (remove-hitbox))
	  
	  (end-time
	   (set-tension :kick-recovery 18)
	   (switch-to-state 'idle :key-buffer key-buffer))))
	
	:tensions-resolved
	(not (tensions-exist :exceptions '(:kick-recovery)))

  :rest
  (progn
  (def-statemeth attack-blocked (other-state)
      (ccnm)
      (setf end-time 65))))

(defmethod-duel character-collision ((cab bodykick) other-state)
	(ccnm)
	(when (or (collision (clashbox cab) other-state))
	  (add-nohit (parent other-state) cab)
      (attack-blocked cab other-state)))

(defstate "spinning-hook-kick"
  :supers
  (clash-attack-box)

  :hb
  multi-hitbox

  :constants
  ((dodge-point 10)
   (dodge-end-point 22)
   (attack-point 28)
   (end-point 65))

  :slots
  ((head-box :initform nil)
  (hip-box :initform nil))

  :initfunc
  ((&key)
    (let ((hip (make-uni-box state 0.0 (* 0.4 (height fighter)) 0.65 0.3))
		   (head (make-uni-box state 0.0 (* 0.7 (height fighter)) 0.5 0.3)))
	 (setf (head-box state) head)
     (setf (hip-box state) hip)
     (setf (hitbox-list state)
	   (list
	   head
	    (make-uni-box state 0.0 0.0 1.0 0.4)
	    hip))))

	:tensions-resolved
	(not (tensions-exist :exceptions '(:rear-pressure)))

  :main-action
  ((common-transitions)
  (when (and (> tpos dodge-point) (<= tpos dodge-end-point))
  (let* ((trunc-time (- tpos dodge-point)) (trunc-end-point (float (- dodge-end-point dodge-point)))
		(ratio (/ trunc-time trunc-end-point)))
     (setf (x hip-box)  (* 0.75 (get-direction) ratio))
     (setf (radius hip-box) (+ 0.65 (* 0.75 ratio)))
	 (setf (x head-box)  (* (width fighter) -0.75 (get-direction) ratio))
     (setf (y head-box) (* (height fighter) (- 0.7 (* 0.2 ratio))))))
   (lcase tpos
	  (attack-point (set-hitbox
			 (make-static-dist-rab
			  :class 'unblockable-box
			  :parent state
			  :damage 140
			  :hitstun 40
			  :hitdist 100
			  :blockstun 40
			  :blockdist 80
			  :x (* (get-direction fighter) 30.0) :Y 52.0
			  :radius 9.0 :height 9.0)))
	  
	  ((+ 2 attack-point) (remove-hitbox))
	  
	  (end-point (switch-to-state 'idle :key-buffer key-buffer)))))