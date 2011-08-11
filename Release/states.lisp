(in-package "KIENEMAN")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants

(defconstant *neutral-leg-space* 8.0)
(defconstant *wide-leg-space* 27.0)
(defconstant *trigger-radius* 0.1)
(defconstant max-step-dist 30.0)
(defconstant *block-startup* 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro default-hitboxes ()
  `(set-hitbox-list (0.0 0.0 1.0 0.4)
		   (0.0 (* 0.4 (height fighter)) 0.65 0.4)
		   (0.0 (* 0.8 (height fighter)) 0.5 0.2)))
		   
(defun get-step-dist ()
"get-butterfly-angle must return a non nil value for this function to work"
  (* (let ((val (* (get-butterfly-angle) 2.0 (/ pi))))
			(min (sqr val) 1.5))
		(get-axis-dist)
		max-step-dist))

(defun valid-step-dist ()
    "Returns true if get-step-dist will return a positive non nil number."
	(and (get-butterfly-angle) (not (equal 0.0 (get-butterfly-angle)))))

(defun rear-avalable ()
	"true if any the rear foot is stable enough to begin moves such as the straight punch."
	t)
	
(defun use-buffered-state (&optional (bs (get-buffered-state)))
	(when (and bs (tensions-resolved bs))
	   (change-state *fighter* bs)
	   (set-buffered-state nil)))

(defun buffered-equal (sym)
	(equal (type-of (get-buffered-state)) sym))

(defmacro common-transitions ()
  `(progn
	 (when (get-pressed :cancel)
			(set-buffered-state nil))
     (cond
	   
	  ((and (get-pressed :a1) (get-in-region :min-butterfly (/ (* pi 1) 8) :max-butterfly (/ (* pi 7) 8)))
       (let ((val (min (/ (get-butterfly-angle) (* pi 0.5)) 1.0)))
	    (format t "~&val: ~a~&ba: ~a~&" val (get-butterfly-angle))
	    (set-buffered-state (make-state 'straight :forward-speed (* 1.5 val) :leg-space (+ *neutral-leg-space* (* 12.0 val))))))
		
	  ((and (get-held :dodge) (get-pressed :a1))
       (set-buffered-state (make-state 'grab)))
	 
      ((and (get-pressed :a1))
       (set-buffered-state (make-state 'jab)))
      
      ;;((or (and (get-tapped :a1) (get-held (direction-symbol))) (and (get-tapped (direction-symbol)) (get-held :a1)))
       ;;(set-buffered-state (make-state 'Kpunch)))
      
      ((and (get-pressed :a2) (get-held :feint))
       (set-buffered-state (make-state 'kick-feint)))
      
      ;; ((and (get-held :a2) (get-tapped (opposite-symbol)))
;;        (set-buffered-state '(forward-hook-kick)))
      
      ((and (get-pressed :a2) ;; (get-tapped (direction-symbol)) (get-held :up)
	    (get-in-region :min-butterfly (/ (* pi 5) 8) :max-butterfly (/ (* pi 7) 8)
				:min-axis-dist *trigger-radius*))
       (set-buffered-state (make-state 'sidekickS)))
      
      ((and (get-pressed :a2)
	    (get-in-region :min-butterfly (/ (* pi 1) 8) :max-butterfly (/ (* pi 3) 8)
				:min-axis-dist *trigger-radius*))
       (set-buffered-state (make-state 'sidekickW)))

      ((and (get-pressed :a2)
	    (get-in-region :min-butterfly (/ (* pi 3) 8) :max-butterfly (/ (* pi 5) 8)
				:min-axis-dist *trigger-radius*))
       (set-buffered-state (make-state 'sidekickW)))
      
      ((and (get-pressed :a2)
	    (get-in-region :min-butterfly (/ (* pi 7) 8)
				:min-axis-dist *trigger-radius*))
       (set-buffered-state (make-state 'bodykick)))
      
      
      
      #|((and (get-pressed :a2) (region-entered-from-center :min-butterfly (/ (* pi 3) 8) :max-butterfly (/ (* pi 5) 8)
				:min-axis-dist *trigger-radius*))
       (set-buffered-state (make-state 'bodykick)))|#
      
      ((and (get-pressed :a2) (get-in-region :max-butterfly (/ (* pi 2) 8)
				:min-axis-dist *trigger-radius*))
       (set-buffered-state (make-state 'spinning-hook-kick)))
	   
	   ((get-pressed :a2)
       (set-buffered-state (make-state 'roundhouse)))
	   
	   #|((and (valid-step-dist) (or (and (get-held :defense) (get-pressed :move)) (and (get-held :move) (get-pressed :defense))))
		)|#
	  
	  ((and (get-pressed :defense)
			(get-in-region :max-butterfly (/ (* pi 2) 8)
				:min-axis-dist *trigger-radius*))
       (set-buffered-state (make-state 'high-block :block-height :gut)))
	   
	   ((get-pressed :defense)
       (set-buffered-state (make-state 'high-block)))

      ((or
			(and (get-pressed :dodge) (get-in-region :min-butterfly (/ (* pi 6) 8) :min-axis-dist *trigger-radius*))
			(and (get-held :dodge) 	  (get-region-entered :min-butterfly (/ (* pi 6) 8) :min-axis-dist *trigger-radius*)))
       (set-buffered-state (make-state 'sidestep)))
      
      ;; ((and (get-held :dodge) (or (get-tapped :r-right) (get-tapped :r-left)))
      ;; 		 (setf buffered-state 'high-dodge :dir dir :pressure (cond ((get-held :down) 0.2) ((get-held :up) 1.0) (t 0.6))
      ;; 				  ;; (get-numeric :throttle)
      ;; 				  ))
	  
	  ((and (get-held :move) (get-region-entered :min-axis-dist *trigger-radius* :min-butterfly 0.01) (valid-step-dist))
		(set-buffered-state (make-state 'continued-step :dir held-dir :total-dist (get-step-dist) :foot-pos (+ 2.0 *neutral-leg-space*))))
		
	  ((and (get-held :dodge) (get-region-entered :min-axis-dist *trigger-radius* :min-butterfly 0.01) (valid-step-dist))
		(set-buffered-state (make-state 'freestep :dir held-dir :total-dist (get-step-dist))))
      
      ((and (get-in-region :min-axis-dist *trigger-radius*)  (valid-step-dist))
       (if (get-pressed :dodge)
	    (set-buffered-state (make-state 'freestep :dir held-dir :total-dist (get-step-dist)))
	   (if (get-pressed :move)
	   (set-buffered-state (make-state 'continued-step :dir held-dir :total-dist (get-step-dist) :foot-pos (+ 2.0 *neutral-leg-space*)))
	    ;(set-buffered-state (make-state 'yukuri-step :dir held-dir :total-dist (get-step-dist)))
		))))))

;;Idle
	  

(defstate "idle"
  :slots
  ((tpos :initform 0)
  (prone-time :initform 0))

  :hb
  multi-hitbox

  :alt-name
  "stride"

  :main-action
  ((flet ((get-untapped (input)
			(if (= (tpos state) 1)
			    (get-tapped input :released t)
			  (get-released input))))
     (let ((dir (if (get-held (direction-symbol)) positive negative)))
		(common-transitions)
		(when (not (get-buffered-state))
		 (cond
			((and (get-held :defense)
			(get-in-region :max-butterfly (/ (* pi 2) 8)
				:min-axis-dist *trigger-radius*))
			(set-buffered-state (make-state 'high-block :block-height :gut)))
			
			((get-held :defense)
			(set-buffered-state (make-state 'high-block)))))
			
	(use-buffered-state)
			
	#|(let ((bs (get-buffered-state)))
       (when (and bs (tensions-resolved bs))
	   (change-state *fighter* bs)
	   (set-buffered-state nil)))|#)))

  :entryfunc
  (progn
  (default-hitboxes)
  (setup-box-display state))

  :rest
  (progn
    (def-statemeth animate ()
      (ccnm)
      (set-time-position animation (/ 8.0 60.0)))
  
    (def-statemeth print-state ()
      (let ((fighter (parent state)))
	(format nil "~at: ~a~%" (ccnm) tpos)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Walking

(defconstant *walkspeed* 0.25)

(defclass+ walkcycle (combined-animation state)
  ((name :initform "walkcycle")))

(let ((walkspeed *walkspeed*))
 (defmethod main-action ((walkcycle walkcycle))
    
   (cond

    ((get-pressed :a1)
     (switch-to-state 'jab))
     
    ((get-held :h-neutral)
     (switch-to-state 'idle))

    ((or
      (and (get-pressed :dodge) (get-held (opposite-symbol)) (get-held :up))
      (and (get-held :dodge)
	   (or (and (get-pressed (opposite-symbol)) (get-pressed :up))
	       (and (get-pressed (opposite-symbol)) (get-held :up))
	       (and (get-held (opposite-symbol)) (get-pressed :up)))))
     (switch-to-state 'backflip))

    ((and (get-held :dodge) (or (get-held :right) (get-held :left)))
     (switch-to-state 'stride :direction (if (get-held (direction-symbol)) positive negative)))
     
    ((get-held :left) 
     (decf (x *fighter*) walkspeed))

    ((get-held :right)
     (incf (x *fighter*) walkspeed))
	 (t (common-transitions)))))

(defmethod animate ((walkcycle walkcycle))
  (call-next-method)
  (if (get-held (direction-symbol *fighter*)) ;; (if (= right (get-direction *fighter*))
;; 					 (get-held :right)
;; 				       (get-held :left))
      (add-time (animation walkcycle) (/ 1.0 150.0))
    (add-time (animation walkcycle) (/ -1.0 150.0))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stunned

(defstate "stunned"

  :slots
  ((duration)
   (kb-direction);;KB stands for knockback
   (kb-speed)
   (decceleration))

  :main-action
  ((decf duration)
   (let ((prev-speed kb-speed))
     (decf-to kb-speed 0.0 decceleration)
     (move-forward 
       (move-forward (/ (+ prev-speed kb-speed) -2.0))
       (target parent)))

   (when (< duration 30) 
   (common-transitions))
   
   (when (<= duration 0)
     (if (<= kb-speed 0.0)
	 (switch-to-state 'idle)
       (switch-to-state 'running
			:vel (- kb-speed)
			:foot-pos *neutral-leg-space*)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Block
(defclass+ blocking ()
  ((name :initform "blocking")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High Block

(defun make-block-tribox (height-symbol &optional (fighter *fighter*))
	(make-instance 'clash-tribox
				   :parent (state fighter)
				   :x (* (get-direction fighter) (if (equal height-symbol :high) 5.0 2.0)) :Y (if (equal height-symbol :high) 43.0 27.0)
				   :scalar-list (list (if (equal height-symbol :high) 4.0 2.0) 20.0  8.0 0.0  0.0 10.0)))

(defstate "high-block"
  :supers
  (single-block-box)

  :hb
  multi-hitbox
  
  :constants
  ((max-reco 15)
  (outside-tension 10))

  :slots
  (;;CAN BE: nil - meaning that there has been no trigger causing the state to change.
	;;		 int - The number of frames before blocking can return to idle.
   (escape-time :initform nil)
   ;;A lambda to be executed when escape time runs out.
   (escape-func :initform nil)
   (block-height :initform :high)
   (block-angle :initform 'outside)
   (block-startup :initform *block-startup*)
   ;;For when in blockstun
   (move-speed :initform 0.0)
   (movement-end-timer :initform 0))

  :initfunc
  ((&key)
  (default-hitboxes))

  :main-action
  ((common-transitions)
  
  (when (> movement-end-timer 0)
	(decf movement-end-timer)
	(move-forward move-speed))
  
  (if escape-time
     (progn
       (decf escape-time)
       (when (<= escape-time 0)
	 (funcall escape-func)))
	 
   (progn
    (if (and (not blockbox) (> tpos 0))
		(set-blockbox (make-block-tribox block-height)))
    ;(if (> tpos block-startup)
	(cond
	 ((and (> tpos block-startup) (<= movement-end-timer 0)
		(or (equal 'straight (type-of (get-buffered-state)))
			(not (get-held :defense))
			(and (equal block-height :high)  (get-in-region :max-butterfly (/ (* pi 2) 8)
																:min-axis-dist *trigger-radius*))
			(and (equal block-height :gut) (not (get-in-region :max-butterfly (/ (* pi 2) 8)
																:min-axis-dist *trigger-radius*)))))
	  (setf escape-time (min tpos max-reco))
	  (setf escape-func #'(lambda () (set-tension :outside-stance outside-tension) (set-minor-tension :lead-hand-use 27) (switch-to-state 'idle)))))
	 
	#|(if (get-pressed :cancel)
	  (progn (setf escape-time (min tpos max-reco))
			(setf escape-func (λ (set-tension :outside-stance outside-tension) (switch-to-state 'idle))))
	 (if (and (valid-step-dist) (get-held :defense) (get-pressed :move))
		(progn (format t "CS~&") (switch-to-state 'continued-step :dir held-dir :total-dist (get-step-dist) :foot-pos (+ 2.0 *neutral-leg-space*)))
	 (common-transitions)))|#
	 ;)
	 )))
	 
	 :rest
	 (progn
	 (def-statemeth block-hit (cab)
		(setf move-speed (/ (- (blockdist cab)) (block-movement-time cab)))
		(setf movement-end-timer (block-movement-time cab))))
	 
	:tensions-resolved
	(not (tensions-exist :exceptions '(:front-pressure :rear-pressure :kick-recovery :outside-stance))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High Block Stun

(defclass+ high-block-stun (high-block)
  ((:ia time-pos
	:initform 0)
   (:iea duration)
   (:iea kb-direction);;KB stands for knockback
   (:iea kb-speed)
   (:iea decceleration)))

(defmethod main-action ((state high-block-stun))
    (with-accessors
     ((time-pos time-pos) (duration duration) (kb-speed kb-speed) (key-buffer key-buffer)
      (kb-direction kb-direction) (decceleration decceleration) (parent parent)) state
     (incf time-pos)
     (decf duration)
     (let ((dir (if (get-held (direction-symbol parent)) positive negative))
			(prev-speed kb-speed))
      (decf-to kb-speed 0.0 decceleration)
      (move-forward 
       (move-forward (/ (+ prev-speed kb-speed) -2.0))
       (target parent))
		(common-transitions))
      
     (cond
      ((<= duration 0)
       ;;Add key-buffer transition to idle
       (if (and (get-held :defense) (get-tapped nil))
	   (switch-to-state 'high-block :key-buffer key-buffer)
	 (switch-to-state 'idle))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grabbed

(defstate "grabbed"
  :alt-name
  "idle"
  
  :slots
  ((grabber))

  :main-action
  ((common-transitions)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stride

(defun stopping-possible-p (vel decceleration distance-left)
  "Given a velocity, a deccel parameter and a distance that must not be
traveled beyond, returns whether stopping is possible within that distance."
  (let* ((speed (abs vel))
	 (unrounded-time (/ speed decceleration))
	 (ceiling-time (ceiling unrounded-time))
	 (floor-time (floor unrounded-time))
	 (lowest-step-h (* decceleration (- unrounded-time floor-time)))
	 (lowest-step-area (* lowest-step-h ceiling-time))
	 (triangle-area (* 0.5 floor-time (- speed lowest-step-h)))
	 (step-top-area (* 0.5 (- speed lowest-step-h)))
	 (total-area (+ lowest-step-area triangle-area step-top-area)))
    (< total-area distance-left)))

(defun slowing-possible-p (vel decell-func foot-pos &optional (no-pass-pos 0.0) (min-speed-limit 0.0))
  "decell-func must take velocity and foot position as arguments and
return a positive real number.
foot-pos and no-pass-pos must be positive."
  (let* ((abs-vel (abs vel))
	 (foot-movement-dir (signum (- no-pass-pos foot-pos)))
	 (new-foot-pos (+ foot-pos (* abs-vel foot-movement-dir)))
	 (new-vel (- vel (* (signum vel) (funcall decell-func vel foot-pos)))))
    (if (not (equal (signum vel) (signum (- new-vel min-speed-limit))))
	t
      (if (not (equal (signum (- no-pass-pos foot-pos)) (signum (- no-pass-pos new-foot-pos))))
	  nil
	(slowing-possible-p new-vel decell-func new-foot-pos no-pass-pos min-speed-limit)))))

(defun slowing-possible-b-p (vel decell-func foot-pos &optional (no-pass-pos 0.0) (min-speed-limit 0.0))
  "decell-func must take velocity and foot position as arguments and
return a positive real number.
foot-pos and no-pass-pos must be positive.
In this function the new foot pos is affected by the new velocity instead of the old one."
  (let* ((new-vel (- vel (* (signum vel) (funcall decell-func vel foot-pos))))
	 (foot-movement-dir (signum (- no-pass-pos foot-pos)))
	 (new-foot-pos (+ foot-pos (* (abs new-vel) foot-movement-dir))))
    (if (not (equal (signum vel) (signum (- new-vel min-speed-limit))))
	(values t new-vel new-foot-pos)
      (if (not (equal (signum (- no-pass-pos foot-pos)) (signum (- no-pass-pos new-foot-pos))))
	  (values nil new-vel new-foot-pos)
	(slowing-possible-b-p new-vel decell-func new-foot-pos no-pass-pos min-speed-limit)))))

(defun stopping-distance (vel decell-func foot-pos &optional (no-pass-pos 0.0) (min-speed-limit 0.0))
  "decell-func must take velocity and foot position as arguments and
return a positive real number.
foot-pos and no-pass-pos must be positive.
In this function the new foot pos is affected by the new velocity instead of the old one."
  (let* ((new-vel (- vel (* (signum vel) (funcall decell-func vel foot-pos))))
	 (foot-movement-dir (signum (- no-pass-pos foot-pos)))
	 (current-dist (* (abs new-vel) foot-movement-dir))
	 (new-foot-pos (+ foot-pos current-dist)))
    ;;If the lower speed limit is reached in the space requirements.
    (if (not (equal (signum vel) (signum (- new-vel min-speed-limit))))
	(values current-dist new-vel new-foot-pos)
      ;;If the max distance is passed.
      (if (not (equal (signum (- no-pass-pos foot-pos)) (signum (- no-pass-pos new-foot-pos))))
	  (values nil new-vel new-foot-pos)
	(stopping-distance new-vel decell-func new-foot-pos no-pass-pos min-speed-limit)))))

(defun test-slowing-possible-p ()
  (format t "~&~a~&" (slowing-possible-p 3.0 #'(lambda (vel foot-pos) 1.25) (+ 3.0 1.75 0.5) 0.45)))

(defun time-till-stop (deccel-func vel foot-pos &optional (min-speed-limit 0.0))
  1.0)

(defmacro push-common-transitions ()
  `(((>= leg-space max-leg-space)
     (format t "Max leg space reached.~&")
     (if (or (get-held :cancel) (not (contracting-possible)))
	 (enter-glide)
       (setf action (create-stoppage-form vel leg-space body-depth))))
    
    ((and (contracting-possible)
	  (or (not (current-dir-held)) (get-pressed :cancel))
	  (get-held :cancel))
     (enter-glide))

    ((and (contracting-possible)
	  (or
	   (and (not (current-dir-held)) (> tpos 3))
	   (get-held :down))
	  (not (get-held :cancel)))
     (setf action (create-stoppage-form vel leg-space body-depth)))))

;;NOTE: velocity must be taken into account eventually.
(defun create-stoppage-form (vel foot-pos body-depth)
  (if (<= foot-pos 23.0)
   (let* ((total-accel (/ (+ foot-pos 21.0) 70.0))
	  (accel-time (round (/ (- foot-pos 3.0) 7.0))))
     (list :sub :stoppage
	   :phase :hopping
	   :tinit accel-time
	   :initial-accel (/ total-accel accel-time)
	   :air-time (round (/ (+ foot-pos 0.0) 12.0))
	   :deccel (/ 4.5 (+ foot-pos 0.0))
	   :stopping-time (round (/ (- foot-pos 4.0) 6.0))))))

(defun create-glide-form (foot-pos)
  (list :sub :glide))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; high-stride

(defconstant *max-stride-vel* 1.9)
(defconstant *max-stride-accel* 0.2)
(defconstant *max-hdash-spd* 1.68)
(defconstant *min-max-leg-space* (+ 7.0 *neutral-leg-space*))

(defun max-foot-pos-per-speed (spd)
  (let ((spd (abs spd))
	)
    (+ *min-max-leg-space* (* (/ spd *max-hdash-spd*) (- *wide-leg-space* *min-max-leg-space*)))))

(defun hs-accel (vel fp)
  "Returns a positive acceleration value."
  (* (/ (- *max-stride-vel* (abs vel)) *max-stride-vel*) *max-stride-accel*))


;;NOTE: The 'dir' &key value should be specified in the case the this state is being enetered from
;;a state which lacks a velocity (any neutral state which keepss the entiry stationary). Dir will
;;specify the direction, and the starting speed will be calculated based on state specific parameters.
;;If this state is initialised with a specific vel (velocity), then the dir &key SHOULD NOT BE USED.
(defstate "high-stride"

  :constants
  ((max-foot-pos *wide-leg-space*)
   (min-speed 0.01))

  :funcs
  ((vel (* dir spd))
   (forward-symbol (if dir
		       (if (>= dir 0.0) (direction-symbol) (opposite-symbol))
		     (if (>= vel 0.0) (direction-symbol) (opposite-symbol))))
   (forward-weight (get-numeric forward-symbol))
   (target-speed (* *max-hdash-spd* forward-weight)))

  :slots
  (;; (vel :initform min-speed)
   (spd :initform nil)
   (dir :initform nil)
   (foot-pos :initform *neutral-leg-space*)
   (body-depth :initform 0.0)
   (expanding :initform positive) ;Can be positive, negative or bridging.
   (speed-lock :initform nil) ;Chosing to move slowly will make this value true, preventing further acceleration.
   (goal-action :initform nil) ;;E.g. If a punch input is recieved,
   ;;but punch cannot yet be preformed, goal-action will become punch to signal he transition.
   (glide-asap :initform nil))

  :alt-name
  "stride"

  :initfunc
  ((&key vel)
   (progn
     (when vel
	 (setf (spd state) (abs vel))
	 (if (not (equal vel 0.0))
	     (setf (dir state) (signum vel))))))

  :entryfunc
  (progn
    (if (not (spd state))
     (setf (spd state) (min target-speed min-speed))))

  :main-action
  ((let* ((forward-symbol (if (>= dir 0.0) (direction-symbol) (opposite-symbol)))
	  (current-dir-held (or (and (> dir 0.0) (get-held (direction-symbol))) (and (< vel 0.0) (get-held (opposite-symbol)))))
	  (abs-speed (abs spd))
	  (leg-delta (* abs-speed expanding))
	  (max-vel *max-stride-vel*)
	  (cap-val (get-numeric forward-symbol))
	  (dash-accel (hs-accel vel foot-pos))
	  (target-weight (get-numeric forward-symbol))
	  (target-speed (* target-weight *max-hdash-spd*))
	  (expanding-possible (slowing-possible-b-p vel #'hs-accel (+ foot-pos leg-delta) *neutral-leg-space* min-speed))
	  (to-negative (or glide-asap (not (get-held :cancel)) (get-held :defense))))

     (flet (;;Keeps the state of the stride consistant, given that it is to be set as negative expansion.
	    (set-expanding-negative ()
				    (setf goal-action nil)
				    (setf expanding negative)
				    (setf speed-lock nil)))
      (if (equal expanding positive)
	  (progn
	    (format t "C1")
	    ;;ALWAYS EXECUTED
	    (incf foot-pos leg-delta)
	    (move-forward (* (abs leg-delta) dir))
	    ;;Acceleration will not occur when attempting to switch to negative expansion,
	    ;;moving into an attack or when the max speed is reached.
	    (when (and (< (abs vel) *max-hdash-spd*) (not to-negative) (not goal-action))
	      (incf-to spd target-speed dash-accel)
	      (when (and (equal spd target-speed) (< target-speed *max-hdash-spd*))
		(setf speed-lock t)))
	    (format t "C2")
	    
	    ;;COND FORM
	    (cond
	     ((get-tapped :a1)
	      (if (>= foot-pos (max-foot-pos-per-speed (abs vel)))
		  (if (get-held :down)
		      (switch-to-state 'forward-jab
				       :vel vel
				       :foot-pos foot-pos)
		    (switch-to-state 'straight
				     :forward-speed vel
				     :leg-space foot-pos))
		(setf goal-action :jab)))

	     ((and (get-held (direction-symbol)) (get-tapped :a2))
	      (switch-to-state 'sidekickW))

	     ((or (get-pressed :cancel) glide-asap)
	      (format t "CA1")
	      (if expanding-possible
		  (switch-to-state 'glide-in
				   :vel (if (if (>= dir 0.0)
						(<= (- vel min-speed) 0.0)
					      (>= (+ vel min-speed) 0.0))
					    (* dir min-speed)
					  vel)
				   :foot-pos foot-pos)
		(setf glide-asap t)))

	     ((equal goal-action :jab)
	      (if (get-held :down)
		      (switch-to-state 'forward-jab
				       :vel vel
				       :foot-pos foot-pos)
		    (switch-to-state 'straight
				     :forward-speed vel
				     :leg-space foot-pos)))
	    
	     ((or (and to-negative expanding-possible) (>= foot-pos (max-foot-pos-per-speed vel)))
	      (format t "TN1 cdh:~a" current-dir-held)
	      (set-expanding-negative))))

	;;If expanding is negative
	(progn
	  ;;Striding backwards will result in a diffrent calculation from the dash accel.
	  ;;By default, this calculation assumes that the leg pos is greater then the neutral position.
	  (decf spd (if (< 0.0 dir) dash-accel dash-accel))
	  (decf foot-pos spd)
	  (move-forward vel)
	  (cond
	  
	   ((or (get-tapped forward-symbol) (get-tapped :cancel))
	    (format t "~&******* TO GLIDE-IN *******~&")
	    (switch-to-state 'glide-in
			     :vel (if (if (>= dir 0.0)
					  (<= (- vel min-speed) 0.0)
					(>= (+ vel min-speed) 0.0))
				      (* dir min-speed)
				    vel)
			     :foot-pos foot-pos))
	  
	   ((<= spd min-speed)
	    (switch-to-state 'idle))
		
		(t (common-transitions))))))

     (format t "~&**************************~&")
     (format t "~&tpos: ~a vel: ~a ~& Foot-Pos ~a Cap-Val: ~a~& expanding: ~a~&" tpos (* abs-speed dir) foot-pos cap-val expanding)))

  :rest
  (progn
    (def-statemeth animate ()
      (ccnm)
      (set-time-position animation (/ foot-pos 60.0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; high-dodge

(defstate "high-dodge"

  :slots
  ((spd :initform 0.15)
   (dir)
   (foot-pos :initform *neutral-leg-space*)
   (starting :initform t)
   (stopping-time :initform 0)
   (air-time :initform 0)
   (start-speed :initform 0.15)
   (air-time-ended :initform nil))

  :animation
  single-animation

  :alt-name
  "stride"

  :funcs
  ((vel (* dir spd))
   (forward-symbol (if (>= dir 0.0) (direction-symbol) (opposite-symbol)))
   (target-weight (get-numeric forward-symbol))
   (max-start-time 5)
   (max-speed (+ (* 1.0) 1.2))
   (accel 0.5))

  :main-action
  ((common-transitions)
  (cond
    (starting
     (incf air-time)
     (incf-to spd max-speed accel)
     (incf foot-pos spd)
     (move-forward vel)
     (when (or (>= air-time max-start-time) ;; (not (get-held :dodge))
	       ;; (not (get-held forward-symbol)) (get-held :down)
	       )
       (let ((new-air-time (round (* 0.7 air-time))))
	 (setf spd (+ spd 0.2 (* air-time 0.01)))
	 (setf stopping-time (round (* 3.0 new-air-time)))
	 (setf air-time new-air-time)
	 (setf starting nil))))

    ((> air-time 0)
     (decf air-time)
     (incf foot-pos spd)
     (move-forward vel))

    ((> stopping-time 0)
     (progn
       (decf-to spd 0.0 0.15)
       (decf-to foot-pos *neutral-leg-space* spd)
       (move-forward vel)
       (decf stopping-time)))

    (t
     (switch-to-state 'idle :key-buffer key-buffer)))

   (format t "~&tpos: ~a spd: ~a ~20Tleg: ~a~& air-time ~a ~20Tstopping-time: ~a" tpos spd foot-pos air-time stopping-time))

  :initfunc
  ((&key (intspeed 0.15))
   (progn
   (format t "in")
    (setf (slot-value state 'spd) intspeed)
    (setf (slot-value state 'start-speed) intspeed)
    (format t "out")))

  :rest
  (progn
    (def-statemeth animate ()
      (set-time-position animation (/ foot-pos 60.0)))))


	  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; freestep

(defstate "freestep"
	:animation
  single-animation
	:alt-name
  "stride"
  
  :supers
  (single-block-box)
  
  :slots
  (;;Input Slots
  (dir)
  (total-dist)
  (entrance-spd :initform 0.0)
  (loseness :initform 0.7)
  ;;Reaction slots
  (covered-dist :initform 0.0)
  (pending-state :initform (list nil nil))
  
  (foot-pos :initform *neutral-leg-space*))
  
  :funcs
  ((interruption-time (if (equal (car pending-state) :cancel) (cadr pending-state) nil))
  (time-mult 20.0)
  (dist-weight (sqrt (/ total-dist 10.0)))
  (pre-time (round (* dist-weight (+ 1 (* time-mult 0.1)))))
  (body-time ;12
  (round (* dist-weight (+ 1 (* time-mult 0.50)))))
  (slow-time ;17
  (round (* dist-weight (+ 1 (* time-mult 0.60)))))
  (final-time (if interruption-time 14 (* dist-weight (+ time-mult 1.0))))
  (end-time (max 1(round final-time)))
  (trans-fact total-dist)
  (spd (if (or (< tpos pre-time) interruption-time)
	(* (/ 0.04 (- pre-time 1.0)) trans-fact)
	(if (< tpos body-time)
	 (* (/ 0.86 (- body-time pre-time)) trans-fact)
	 (if (< tpos slow-time) (* (/ 0.1 (- slow-time body-time)) trans-fact) (* 0.0 trans-fact)))))
  (vel (* dir spd)))
  
  :main-action
  ((cond
    ((and (get-pressed :cancel) (< tpos pre-time) (not interruption-time)) ;;If cancel is sucessful.
		(setf pending-state (list :cancel tpos)))
	
	((and (get-pressed :defense) (null (car pending-state)))
		(setf pending-state (list :defense tpos)))
		
	 (t (common-transitions)))
	 
  (move-forward vel)
  (incf covered-dist spd)
  
  (when (< tpos pre-time)
   (incf foot-pos spd))
  (when (>= tpos body-time)
   (decf foot-pos spd))
	
	(when (>= tpos slow-time)
	 (cond
	 ((and (not blockbox) (= tpos slow-time) (equal (car pending-state) :defense))
		 (set-blockbox (make-block-tribox :high)))))
				   
	(if (= end-time 0) (switch-to-state 'idle);The case that zero distance was given.
		;else
		(lcase tpos
			(end-time
			(format t "~&CD: ~a tpos: ~a ~&" covered-dist tpos)
			(if (equal (car pending-state) :defense) (progn (switch-to-state 'high-block :tpos (- tpos (cadr pending-state))))
				(progn
				(if (equal dir positive)
					(set-tension :front-pressure (round (* dist-weight 5)))
					(set-tension :rear-pressure (round (* dist-weight 5))))
				(switch-to-state 'idle)))))))
				
	:tensions-resolved
	(not (tensions-exist :exceptions '(:front-pressure :rear-pressure)))
  
  :rest
  (progn
    (def-statemeth animate ()
      (ccnm)
      (set-time-position animation (/ foot-pos 60.0)))))

(defstate "yukuri-step"
	:animation
		single-animation
	:alt-name
		"stride"
	:supers
	(single-block-box)
	
	:slots
	(;;Input Slots
  (dir)
  (total-dist)
  (entrance-spd :initform 0.0)
  (loseness :initform 0.7)
  ;;Reaction slots
  (covered-dist :initform 0.0)
  (pending-state :initform (list nil nil))
  
  (foot-pos :initform *neutral-leg-space*))
	
	:funcs
  ((interruption-time (if (equal (car pending-state) :cancel) (cadr pending-state) nil))
  (time-mult 20.0)
  (dist-weight (sqrt (/ total-dist 10.0)))
  (pre-time (round (* dist-weight (+ 1 (* time-mult 0.25)))))
  (body-time ;12
  (round (* dist-weight (+ 1 (* time-mult 0.55)))))
  (slow-time ;17
  (round (* dist-weight (+ 1 (* time-mult 0.72)))))
  (final-time (if interruption-time 14 (* dist-weight (+ time-mult 2.0))))
  (end-time (round final-time))
  (trans-fact total-dist)
  (spd (if (or (< tpos pre-time) interruption-time)
	(* (/ 0.2 (- pre-time 1.0)) trans-fact)
	(if (< tpos body-time)
	 (* (/ 0.5 (- body-time pre-time)) trans-fact)
	 (if (< tpos slow-time) (* (/ 0.3 (- slow-time body-time)) trans-fact) (* 0.0 trans-fact)))))
  (vel (* dir spd))
  (dist-to-stop (- total-dist covered-dist))
  (avg-remaining-speed (if (>= tpos slow-time) 0.0 (/ dist-to-stop (- slow-time tpos)))))
	
	:main-action
	((cond
	 ((and (get-pressed :cancel) (< tpos pre-time) (not interruption-time)) ;;If cancel is sucessful.
		(setf pending-state (list :cancel tpos)))
	 ((and (get-pressed :move) (null (car pending-state))) ;;If move is pressed.
		(if (< tpos pre-time)
			(if (valid-step-dist) (switch-to-state 'continued-step :dir dir :total-dist (get-step-dist) :foot-pos foot-pos))
			(if (< tpos slow-time)
			 (progn (if (and (get-pressed :move) (equal held-dir dir) (valid-step-dist)) (setf pending-state (list :continue slow-time (get-step-dist))) (common-transitions))) (common-transitions))))
	 ((and (get-pressed :defense) (null (car pending-state)))
		(if (and (get-held :move) (< tpos 3) (valid-step-dist))
			(switch-to-state 'continued-step :dir dir :total-dist (get-step-dist) :foot-pos foot-pos)
		 (setf pending-state (list :defense tpos))))
	 ((and  (get-pressed :a2)
			(<= tpos slow-time)
			(get-in-region :min-butterfly (/ (* pi 1) 8) :max-butterfly (/ (* pi 5) 8) :min-axis-dist *trigger-radius*) 
			(null (car pending-state)))
		(setf pending-state (list :sidekickw tpos)))
	 ((and (get-pressed :a1) (get-in-region :max-axis-dist *trigger-radius*)
			(equal tpos slow-time) (null (car pending-state)))
		(setf pending-state (list :jab tpos)))
	 (t (common-transitions)))
	 
  (move-forward vel)
  (incf covered-dist spd)
  
  (when (< tpos pre-time)
   (incf foot-pos spd))
  (when (>= tpos body-time)
   (decf foot-pos spd))
	
	(cond
	((and (equal (car pending-state) :continue) (equal tpos (cadr pending-state)))
		(switch-to-state 'continued-step :dir dir :total-dist (caddr pending-state) :foot-pos foot-pos))
	 ((and (equal (car pending-state) :sidekickw) (equal tpos slow-time))
		(switch-to-state 'sidekickw))
	 ((and (equal (car pending-state) :jab) (equal tpos slow-time))
		(switch-to-state 'jab))
	 ((and (not blockbox) (= tpos slow-time) (equal (car pending-state) :defense))
		 (set-blockbox (make-block-tribox :high))))
				   
	(if (= final-time 0) (switch-to-state 'idle)
		;else
		(lcase tpos
			(end-time
			(format t "~&CD: ~a tpos: ~a ~&" covered-dist tpos)
			(if (equal (car pending-state) :defense) (switch-to-state 'high-block :tpos (- tpos (cadr pending-state)))
			(progn
				(set-tension :front-pressure 15)
				(switch-to-state 'idle)))))))
				
	:tensions-resolved
	(not (tensions-exist :exceptions '(:front-pressure)))
	
	:rest
	(progn
     (def-statemeth animate ()
      (ccnm)
      (set-time-position animation (/ foot-pos 60.0)))))
	  
(defstate "continued-step"
	:animation
  single-animation
	:alt-name
  "stride"
  
  :slots
  (;;Input Slots
  (dir)
  (total-dist)
  (entrance-spd :initform 0.0)
  (loseness :initform 0.7)
  ;;Reaction slots
  (covered-dist :initform 0.0)
  (foot-pos :initform *neutral-leg-space*)
  (pending-state :initform (list nil nil 0)))
  
  :funcs
  ((dist-weight (sqrt (/ total-dist 10.0)))
  (pre-time (round (* 5.0 dist-weight)))
  (body-time (round (* 20.0 dist-weight)))
  (final-time (round (* 22.0 dist-weight)))
  (end-time (max 1 (round (* 28.0 dist-weight))))
  (spd (/ (* 1.5 total-dist) final-time))
   (vel (* dir spd)))
  
  :main-action
  ((when (<= tpos final-time)
    (move-forward vel)
    (incf covered-dist spd))
  
  (when (and (< tpos pre-time) (> foot-pos 1.0))
   (decf foot-pos spd) (if (< foot-pos 1.0) (setf foot-pos 1.0)))
  (when (and (>= final-time tpos body-time))
   (incf foot-pos (* 0.7 spd)) (if (> foot-pos 30.0) (setf foot-pos 30.0)))
   
   #|(if (and (get-pressed :move) (< tpos body-time) (valid-step-dist) (equal dir held-dir))
		(if (not (first pending-state)) (setf pending-state (list 'continue (get-step-dist) 0)) (setf (third pending-state) 1))
		 (common-transitions))|#
	(common-transitions)
   
   (cond
		((and (or (= tpos body-time) (= tpos final-time)) (equal dir positive) (buffered-equal 'sidekickW))
			(use-buffered-state))
   
		((and (or (and (equal (type-of (get-buffered-state)) 'continued-step) (equal dir (dir (get-buffered-state))))
				(and (get-held :move) (valid-step-dist) (equal dir held-dir) (not (get-buffered-state))))
			(equal tpos body-time))
		(progn (setf tpos 0) (setf covered-dist 0.0) (setf total-dist (or (and (equal (type-of (get-buffered-state)) 'continued-step) (total-dist (get-buffered-state))) (get-step-dist)))
			(setf foot-pos (+ (* 0.3 total-dist) *neutral-leg-space*))
			(set-buffered-state nil)))
			
		((and (get-held :move) (null (get-buffered-state)) (valid-step-dist) (equal (opposite dir) held-dir))
			(progn (set-buffered-state (make-state 'continued-step :dir held-dir :total-dist (get-step-dist) :foot-pos (+ 2.0 *neutral-leg-space*)))))
		
		(t (lcase tpos
		    (end-time
			(if (equal dir positive)
			(set-tension :front-pressure 20)
			(set-tension :rear-pressure 20))
			(switch-to-state 'idle)))))
   
   
	(format t "~&f-p: ~a ~a~&" total-dist (type-of (get-buffered-state))))
	
  :tensions-resolved
  (not (tensions-exist :exceptions '(:front-pressure :front-leg-forward :rear-pressure :kick-recovery)))
  
  :rest
  (progn
    (def-statemeth animate ()
      (ccnm)
      (set-time-position animation (/ foot-pos 60.0)))))
	  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; running

(defstate "running"

  :supers
  (component-velocity)

  :constants
  ((max-speed 1.7)
   (max-accel 0.08)
   (max-running-weight 15)
   (min-running-weight 5))

  :slots
  (;; (spd)
;;    (dir)
   (foot-pos)
   ;; (key-buffer :initform nil)
   (running-weight :initform min-running-weight)	;;The amount of time it would take to return to a stride.
   (weight-limit-met :initform nil)
   (time-till-return :initform 0)
   (returning-state :initform nil)
   (animation-incr :initform 0.2))

  :alt-name
  "walkcycle"
  
  :main-action
  ((let* ((forward-symbol (if (> dir 0.0) (direction-symbol) (opposite-symbol)))
	  (running-weight-limit (round (+ min-running-weight (* (- max-running-weight min-running-weight) (/ spd max-speed)))))
	  (dir-held (or (and (> dir 0.0) (get-held (direction-symbol))) (and (< dir 0.0) (get-held (opposite-symbol)))))
	  (vel (* spd dir))
	  (attempt-return (or (not dir-held) (get-held :defense)))
	  (accel max-accel)
	  (forward-weight (get-numeric forward-symbol))
	  (target-speed (* max-speed forward-weight)))

     (labels ( ;; (get-tapped (input &optional un)
	      ;; 			  (or (equal input key-buffer) (get-pressed input)))
	      ;; (set-tapped (input &optional un)
	      ;; 			  (let ((info (if un input `(:un ,input))))
	      ;; 			    (setf keybuffer info)))
	      (speed-to-target (target-spd &optional (accel max-accel))
			       (if (<= spd target-spd)
				   (incf-to spd target-spd accel)
				 (decf-to spd target-spd accel)))
	      (enter-return ()
			    (setf returning-state :to-stop)
			    (setf time-till-return running-weight))
	      (enter-neutral ()
			     (setf returning-state nil)
			     (setf time-till-return running-weight))
	      (enter-straight ()
			      (setf returning-state :to-straight)
			      (setf time-till-return running-weight))
	      (enter-grab ()
			      (setf returning-state :to-grab)
			      (setf time-till-return running-weight))
	      (enter-glide-in ()
			      (setf returning-state :to-glide-in)
			      (setf time-till-return running-weight))
	      (enter-frontkick ()
			       (setf returning-state :to-frontkick)
			       (setf time-till-return running-weight))
	      (enter-sidekick ()
			      (setf returning-state :to-sidekick)
			      (setf time-till-return running-weight)))

       (when (get-pressed :a1)
	 (if (get-held :defense)
	     (enter-grab)
	   (progn
	     (set-tapped :a1)
	     (enter-straight))))

       (when (get-pressed :defense)
	 (if (get-held :a1)
	     (enter-grab)
	   (enter-neutral)))

       (when (get-pressed :a2)
	 (set-tapped :a2)
	 (enter-frontkick))

       (when (get-pressed :dodge)
	 (set-tapped :dodge)
	 (enter-straight))

       (when (get-pressed :cancel)
	 (set-tapped :cancel)
	 (enter-glide-in))
     
       (move-forward vel)
       
       (cond
	((and (not attempt-return) (or (not returning-state) (equal returning-state :to-stop)))
	 (progn
	   (incf-to running-weight running-weight-limit 1 :force-to-limit t)
	   ;;Speed increases should only occur when the appropriate weight limit is reached.
	   (when (>= running-weight running-weight-limit)
	     (setf weight-limit-met t))
	   (when weight-limit-met
	     (format t "~&Target-Speed: ~a Forward-Weight ~a Forward-Symbol ~a~&" target-speed forward-weight forward-symbol)
	     (speed-to-target target-speed))
	   (when (equal returning-state :to-stop) (enter-neutral))))
	((and attempt-return (or (not returning-state) (equal returning-state :to-stop)))
	 (if (equal returning-state :to-stop)
	     (let ((accel max-accel))
		
	       (decf-to spd 0.1 accel)
	       (decf-to time-till-return 0)
	       (when (and (= time-till-return 0) (<= spd 0.1))
		 ;; (switch-to-state high-stride :vel (* spd dir) :foot-pos foot-pos :expanding negative)
		 (switch-to-state 'idle :key-buffer key-buffer)))
	     
	   ;;If not returning to dash
	   (progn
	     (enter-return))))
	
	((equal returning-state :to-straight)
	 (speed-to-target 1.6)
	 (decf-to time-till-return 0)
	 (when (= time-till-return 0)
	   ;; (switch-to-state straight :forward-speed (* spd dir) :leg-space foot-pos)
	   (switch-to-state 'high-stride :vel (* spd dir) :foot-pos foot-pos :key-buffer key-buffer)))

	((equal returning-state :to-grab)
	 (speed-to-target 0.0)
	 (decf-to time-till-return 0)
	 (when (and (= time-till-return 0) (<= spd 0.0))
	   (switch-to-state 'grab :key-buffer key-buffer)))

	((equal returning-state :to-frontkick)
	 (speed-to-target 0.2)
	 (decf-to time-till-return 0)
	 (when (and (= time-till-return 0) (<= spd 0.2))
	   ;; (switch-to-state straight :forward-speed (* spd dir) :leg-space foot-pos)
	   (switch-to-state 'frontkick :vel (* spd dir) :foot-pos (+ 4.0 *neutral-leg-space*) :key-buffer key-buffer)))

	((equal returning-state :to-sidekick)
	 (speed-to-target target-speed)
	 (decf-to time-till-return 0)
	 (when (= time-till-return 0)
	   (switch-to-state 'sidekick :start-vel (* spd dir) :foot-pos foot-pos)))

	((equal returning-state :to-glide-in)
	 (speed-to-target 1.6)
	 (decf-to time-till-return 0)
	 (when (= time-till-return 0)
	   (switch-to-state 'glide-in :vel (* spd dir) :foot-pos (+ 5.0 *neutral-leg-space*)))))

       (setf animation-incr (* vel 0.5))
       
       (format t "~&**********************************~&")
       (format t "vel: ~a ~20Tleg: ~a~& running-weight: ~a ~20Tweight-limit-met: ~a~&r-state ~a ~&ttr: ~a ~10Tdir: ~a"
	       vel foot-pos running-weight weight-limit-met returning-state time-till-return dir)))))

(defconstant *min-foot-pos* 1.0)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sidestep

(defstate "sidestep"

  :funcs
  ((vel (* -1.0 spd))
   (start-time 6)
   (stop-time 20)
   (end-time 38))

  :slots
  ((spd :initform 0.5)) ;This should be constant throughout the life of this state.

  :main-action
  ((when (and (>= tpos start-time) (< tpos stop-time)) (move-forward vel))
	(common-transitions)
   (when (>= tpos end-time)
     (switch-to-state 'idle :key-buffer key-buffer)))
	
  :tensions-resolved
  (not (tensions-exist :exceptions '(:kick-recovery)))

  :rest
  (progn
  (def-statemeth animate ()
      (when (< tpos stop-time) (call-next-method)))
  (def-statemeth side-spd ()
    (if (and (>= tpos start-time) (< tpos stop-time)) 2.0 0.0))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backflip

(defstate "backflip"
  
  :constants
  ((flip-start 3)
   (flip-end 16))

  :animation
  multi-animation
  
  :main-action
  ((common-transitions)
  (within flip-start flip-end
	   (move-forward -3.5))
   
   (when (equal tpos 25)
	 (switch-to-state 'idle)))

  :initfunc
  ((&key)
   (progn
	(setf (anim-list state) (list :anim  (make-instance 'single-animation :name "backflip")))))

  :rest
  (def-statemeth y ()
    "Causes the character to move their hitbox up into the air
in the middle of the flip."
    (if (within flip-start flip-end)
	(+ (call-next-method) (* (height state) 0.5))
      (call-next-method))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstate "duck"

	:slots
	((end-time :initform 12))

  :funcs
  ((holding (get-held :down)))
  
  :main-action
  ((if (and (= 11 tpos) holding)
       (setf tpos 10)
	   (common-transitions))

   (when (= tpos 20)
     (switch-to-state 'idle :key-buffer key-buffer)))

  :rest
  (progn
    (def-statemeth height ()
      (if (and (> tpos 3) (< tpos 16))
	      (- (call-next-method) 20)
	(call-next-method)))

    (def-statemeth animate ()
      (ccnm)
      (set-time-position animation (float (/ tpos 60.0))))))