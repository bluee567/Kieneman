(in-package "KIENEMAN")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants

(defconstant *neutral-leg-space* 8.0)
(defconstant *wide-leg-space* 27.0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro default-hitboxes ()
  `(set-hitbox-list (0.0 0.0 1.0 0.4)
		   (0.0 (* 0.4 (height fighter)) 0.65 0.4)
		   (0.0 (* 0.8 (height fighter)) 0.5 0.2)))

;;Idle
(let ((ht nil)
		(va 0.0))
(defstate "idle"
  :slots
  ((tpos :initform 0))

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
       (cond
	((or
	  (and (get-tapped :dodge) (get-held (opposite-symbol)) (get-held :up))
	  (and (get-held :dodge)
	       (or (and (get-tapped (opposite-symbol)) (get-tapped :up))
		   (and (get-tapped (opposite-symbol)) (get-held :up))
		   (and (get-held (opposite-symbol)) (get-tapped :up)))))
	 (switch-to-state 'backflip))

	((get-untapped :a1)
	 (switch-to-state 'jab)
	 (if ht
	 (progn (incf va 10.0) (update-hit-triangle (- va) 0.0  100.0 0.0  0.0 50.0 ht))
	 (setf ht (make-hit-triangle 0.0 0.0  100.0 0.0  0.0 50.0 "red" *mgr*))))

	((and (get-held :a2) (get-tapped (direction-symbol)))
	 (switch-to-state 'sidekickW))

	((and (get-held :a2) (get-tapped (opposite-symbol)))
	 (switch-to-state 'sidekickS))

	((and (get-held :up) (get-tapped :a2))
	 (switch-to-state 'roundhouse))

	((and (get-held :a1) (get-tapped :a2))
	 (switch-to-state 'bodykick))

	((and (get-held :a2) (get-held :down))
	 (switch-to-state 'spinning-hook-kick))

	((and (get-held :defense) (get-held :a1))
	 (switch-to-state 'grab))
      
	((get-held :defense)
	 (switch-to-state 'high-block))

	((and (get-held :dodge) (get-held :down))
	 (switch-to-state 'duck))

	((and (get-held :dodge) (get-held :up))
	 (switch-to-state 'sidestep))
      
	((not (get-held :h-neutral))
	 (if (not (get-held :dodge))
	     (switch-to-state 'high-stride :dir dir)
	       
	   (switch-to-state 'high-dodge :dir dir)))))))

  :entryfunc
  (default-hitboxes)

  :rest
  (progn
    (def-statemeth animate ()
      (ccnm)
      (set-time-position animation (/ 8.0 60.0)))
  
    (def-statemeth print-state ()
      (let ((fighter (parent state)))
	(format nil "~at: ~a~%" (ccnm) tpos))))))


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
     (incf (x *fighter*) walkspeed)))))

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
     (move-forward (- (/ (+ prev-speed kb-speed) 2))))

   (when (get-released :a1)
       (set-untapped :a1))
     (when (get-released :a2)
       (set-untapped :a2))
     (when (get-pressed :defense)
       (set-tapped nil))
   
   (when (<= duration 0)
     (switch-to-state 'idle :key-buffer key-buffer))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Block
(defclass+ blocking ()
  ((name :initform "blocking")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High Block

(defstate "high-block"
  :supers
  (blocking)

  :hb
  multi-hitbox

  :slots
  ((escape-time :initform nil) ;;The number of frames before blocking can return to idle.
   (escape-func :initform nil)) ;;A lambda to be exacuted when escape time runs out.

  :entryfunc
  (set-hitbox-list (0.0 0.0 1.0 0.4)
		   (0.0 (* 0.4 (height fighter)) 1.0 0.3)
		   (0.0 (* 0.7 (height fighter)) 1.8 0.3))

  :main-action
  ((if escape-time
     (progn
       (decf escape-time)
       (when (<= escape-time 0)
	 (funcall escape-func)))
     (when (> tpos 6)
       (cond
	((not (get-held :defense))
	 (setf escape-time 4)
	 (setf escape-func (λ (switch-to-state 'idle :key-buffer key-buffer))))
	
	((get-held :a1)
	 (setf escape-time 3)
	 (setf escape-func (λ (switch-to-state 'grab))))
	
	((get-pressed :down)
	 (setf escape-time 2)
	 (setf escape-func (λ (switch-to-state 'duck)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High Block Stun

(defclass+ high-block-stun (high-block)
  ((:ia time-pos
	:initform 0)
   (:iea duration)
   (:iea kb-direction);;KB stands for knockback
   (:iea kb-speed)
   (:iea decceleration)))

(defmethod main-action ((high-block-stun high-block-stun))    
    (with-accessors
     ((time-pos time-pos) (duration duration) (kb-speed kb-speed)
      (kb-direction kb-direction) (decceleration decceleration) (key-buffer key-buffer)) high-block-stun
     (incf time-pos)
     (decf duration)
     (let ((prev-speed kb-speed))
      (decf-to kb-speed 0.0 decceleration)
      (move-forward (/ (+ prev-speed kb-speed) -2.0)))
     

     (when (get-released :a1)
       (set-untapped :a1))
     (when (get-released :a2)
       (set-untapped :a2))
     (when (get-tapped :down)
       (set-tapped :down))
     (when (get-pressed :defense)
       (set-tapped nil))
      
     (cond
      ((<= duration 0)
       ;;Add key-buffer transition to idle
       (if (and (get-held :defense) (get-tapped nil))
	   (switch-to-state 'high-block :key-buffer key-buffer)
	 (switch-to-state 'idle :key-buffer key-buffer))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grabbed

(defstate "grabbed"
  :alt-name
  "idle"
  
  :slots
  ((grabber))

  :main-action
  ())


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
   (;; (setf (vel state) (* dir (min target-speed (vel state)))
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
	  (to-negative (or glide-asap (not current-dir-held) (get-held :defense))))

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

	     ((or (get-tapped :cancel) glide-asap)
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
	  (format t "D1")
	  (decf spd (if (< 0.0 dir) dash-accel dash-accel ;; (/ (- spd min-speed) ;; (signum vel)
;; 						  2.0 (- foot-pos *neutral-leg-space*))
			))
	  (decf foot-pos spd)
	  (move-forward vel)
	  (format t "D2")
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
	   ;; ((get-tapped :a1)
;; 	    (switch-to-state forward-jab
;; 			     :vel vel
;; 			     :foot-pos foot-pos))
	  
	   ((<= spd min-speed)
	    (format t "I1")
	    (switch-to-state 'idle))))))

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
  ((cond
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
   ((format t "in")
    (setf (slot-value state 'spd) intspeed)
    (setf (slot-value state 'start-speed) intspeed)
    (format t "out")))

  :rest
  (progn
    (def-statemeth animate ()
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

  ;; :initfunc
;;   ((&key vel)
;;    ((setf (spd state) (abs vel))
;;     (setf (dir state) (signum vel))))
  
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
;; Glide

(defstate "glide-in"

  :slots
  ((vel)
   (foot-pos)
   (status :initform :hori) ;;Can be hori or an integer number.
   (exit-height :initform nil)
   (min-foot-pos :initform *min-foot-pos*))

  :alt-name
  "stride"

  :animation
  single-animation

  :funcs
  ((dir (signum vel))
   (forward-symbol (if dir
		       (if (>= dir 0.0) (direction-symbol) (opposite-symbol))
		     (if (>= vel 0.0) (direction-symbol) (opposite-symbol))))
   (forward-weight (get-numeric forward-symbol))
   (target-speed (* *max-hdash-spd* forward-weight)))

  :main-action
  ((move-forward vel) ;;Always occurs.

   (when (get-pressed :a1)
     (set-tapped :a1))

   (when (get-pressed :a2)
     (set-tapped :a2))

   (when (get-pressed :dodge)
	 (set-tapped :dodge))
   
   (when (get-pressed :cancel)
     (set-tapped :cancel))

   (let ((min-spd (* 0.8 (/ (- *neutral-leg-space* min-foot-pos) *neutral-leg-space*))))
     (format t "~&** Min-Spd ~a **~&" min-spd)
    (if (> vel 0.0)
	(incf-to vel (max min-spd (min 1.4 target-speed)) 0.06)
      (decf-to vel (min (- min-spd) (max -1.4 (- target-speed))) 0.06)))

   (when (equal status :hori)
    (decf foot-pos (abs vel))

    (when (<= foot-pos min-foot-pos)
     (format t "Min leg pos reached.~&")
     (setf foot-pos min-foot-pos)
     (let ((sval (round (* 10 (/ (abs vel) *max-stride-vel*)))))
      (setf status sval)
      (setf exit-height sval))))

   (when (not (equal status :hori))
     (decf-to status 0)
     (if (> vel 0.0)
	(decf-to vel (min 1.1 target-speed) 0.05)
      (incf-to vel (max -1.1 (- target-speed)) 0.05))
     (if (<= status 0)
	(switch-to-state 'glide-out
			 :vel vel
			 :foot-pos foot-pos
			 :entry-height exit-height
			 :max-foot-pos  (* (- *wide-leg-space* *neutral-leg-space*) (/ (abs (vel state)) *max-stride-vel*)) ;; (- (* 2 *neutral-leg-space*) foot-pos)
			 :key-buffer key-buffer)))

   (format t "~&**************************~&")
   (format t "~&tpos: ~a Vel: ~a ~& Foot-Pos ~a Min-Foot-Pos: ~a~&" tpos vel foot-pos min-foot-pos))

  :initfunc
  ((&key)
   ((format t "~&** fp: ~a **~~&*~&" (foot-pos state))
    (if (<= (foot-pos state) *neutral-leg-space*)
	(progn
	  (setf (foot-pos state) (+ *neutral-leg-space* 1.0))
	  (setf (min-foot-pos state) (- *neutral-leg-space* 1.0)))
      (setf (min-foot-pos state)
	    (- *neutral-leg-space* (* *neutral-leg-space*
				      (max (/ (- (foot-pos state) *neutral-leg-space*) (- *wide-leg-space* *neutral-leg-space*)) ;Foot closeness value
					   (/ (abs (vel state)) *max-stride-vel*)))))))) ; Speed

  :rest
  (progn
    (def-statemeth animate ()
      (set-time-position animation (/ foot-pos 60.0)))))

(defstate "glide-out"

  :alt-name
  "stride"

  :slots
  ((vel)
   (foot-pos)
   (entry-height);;A higher value of this will make it possible to transfer into heavier attacks.
   (max-foot-pos))

  :animation
  single-animation

  :funcs
  ((dir (signum vel))
   (forward-symbol (if dir
		       (if (>= dir 0.0) (direction-symbol) (opposite-symbol))
		     (if (>= vel 0.0) (direction-symbol) (opposite-symbol))))
   (forward-weight (get-numeric forward-symbol))
   (target-speed (* *max-hdash-spd* forward-weight)))

  :main-action
  ((let* ((forward-symbol (if (>= vel 0.0) (direction-symbol) (opposite-symbol))))
    
    (incf foot-pos (abs vel))
    (move-forward vel)
    (if (get-held forward-symbol)
      (if (> vel 0.0)
	  (incf-to vel (min target-speed 0.8) 0.02)
	(decf-to vel (max (- target-speed) -0.8) -0.02))
      (if (> vel 0.0)
	  (decf-to vel 0.0 0.02)
	(incf-to vel 0.0 -0.02)))

    (when (get-pressed :a1)
	 (set-tapped :a1))

    (when (get-pressed :a2)
      (set-tapped :a2))

    (when (get-pressed :dodge)
	 (set-tapped :dodge))
    
   (when (get-pressed :cancel)
     (set-tapped :cancel))

    (when (>= foot-pos max-foot-pos) 
      (format t "Min leg pos reached.~&")
     
					;Switch to high-stride
      (switch-to-state 'high-stride
		       :vel vel
		       :foot-pos foot-pos
		       :key-buffer key-buffer))

    (format t "~&**************************~&")
    (format t "~&tpos: ~a Vel: ~a ~& Foot-Pos ~a~&" tpos vel foot-pos)))

  :rest
  (progn
    (def-statemeth animate ()
      (set-time-position animation (/ foot-pos 60.0)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sidestep

(defstate "sidestep"

  :funcs
  ((vel (* -1.0 spd)))

  :slots
  ((spd :initform 0.6)) ;his should be constant throughout the life of this state.

  :main-action
  ((move-forward vel)
   (when (>= tpos 20)
     (switch-to-state 'idle :key-buffer key-buffer)))

  :rest
  (def-statemeth side-spd ()
    2.0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backflip

(defstate "backflip"
  
  :constants
  ((flip-start 3)
   (flip-end 16))

  :animation
  multi-animation
  
  :main-action
  ((within flip-start flip-end
	   (move-forward -3.5))
   
   (when (equal tpos 25)
	 (switch-to-state 'idle)))

  :initfunc
  ((&key)
   ((setf (anim-list state) (list :anim  (make-instance 'single-animation :name "backflip")
				  ))))

  :rest
  (def-statemeth y ()
    "Causes the character to move their hitbox up into the air
in the middle of the flip."
    (if (within flip-start flip-end)
	(+ (call-next-method) (* (height state) 0.5))
      (call-next-method))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstate "duck"

  :funcs
  ((holding (get-held :down)))
  
  :main-action
  ((if (and (= 11 tpos) holding)
       (setf tpos 10))

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