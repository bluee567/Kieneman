(in-package "KIENEMAN")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants

(defconstant *neutral-leg-space* 8.0)
(defconstant *wide-leg-space* 27.0)
(defconstant *trigger-radius* 0.75)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defmacro common-transitions ()
  `(progn
     ;; (when (get-pressed :a1)
;;        (set-tapped :a1))
     
;;      (when (get-pressed :a2)
;;        (set-tapped :a2))
     
;;      (when (get-pressed :dodge)
;;        (set-tapped :dodge))
     
;;      (when (get-pressed :cancel)
;;        (set-tapped :cancel))

;;      (when (or (get-held :a1) (get-held :a2) (get-held :dodge))
;;        (when (get-pressed :r-right)
;; 	 (set-tapped :r-right))
;;        (when (get-pressed :r-left)
;; 	 (set-tapped :r-left)))

     ;;Copy of the case statement in idle.
     ;; (format t "~&**ba: ~a ad: ~a**~&" (get-butterfly-angle) (get-axis-dist))
     (cond
      ((and (get-tapped :a1) (get-held :up))
       (set-buffered-state '(jab)))
      
      ((or (and (get-tapped :a1) (get-held (direction-symbol))) (and (get-tapped (direction-symbol)) (get-held :a1)))
       (set-buffered-state '(Kpunch)))
      
      ((and (get-pressed :a2) (get-held :feint))
       (set-buffered-state '(kick-feint)))
      
      ;; ((and (get-held :a2) (get-tapped (opposite-symbol)))
;;        (set-buffered-state '(forward-hook-kick)))
      
      ((and (get-held :a2) ;; (get-tapped (direction-symbol)) (get-held :up)
	    (region-entered-from-center :min-butterfly (/ (* pi 5) 8) :max-butterfly (/ (* pi 7) 8)
				:min-axis-dist *trigger-radius*))
       (set-buffered-state '(sidekickS)))
      
      ((and (get-held :a2) ;; (get-tapped (direction-symbol)) (get-held :down)
	    (region-entered-from-center :min-butterfly (/ (* pi 1) 8) :max-butterfly (/ (* pi 3) 8)
				:min-axis-dist *trigger-radius*))
       (set-buffered-state '(sidekickW)))

      ((and (get-pressed :a2) ;; (get-tapped (direction-symbol))
	    (get-in-region :min-butterfly (/ (* pi 3) 8) :max-butterfly (/ (* pi 5) 8)
				:min-axis-dist *trigger-radius*))
       (set-buffered-state '(sidekickM)))
      
      ((and (get-held :a2) ;; (get-tapped (direction-symbol))
	    (region-entered-from-center :min-butterfly (/ (* pi 3) 8) :max-butterfly (/ (* pi 5) 8)
				:min-axis-dist *trigger-radius*))
       (set-buffered-state '(bodykick)))
      
      ((or (and (get-held :a2)  (or (get-pressed :up)
				    (region-entered-from-center :min-butterfly (/ (* pi 7) 8)
							     :min-axis-dist *trigger-radius*)))
	   (and (get-pressed :a2) (or (get-held :up)
				      (get-in-region :min-butterfly (/ (* pi 7) 8)
						     :min-axis-dist *trigger-radius*))))
       (set-buffered-state '(roundhouse)))
      
      ((and (get-held :a2) (region-entered-from-center :min-butterfly (/ (* pi 3) 8) :max-butterfly (/ (* pi 5) 8)
				:min-axis-dist *trigger-radius*))
       (set-buffered-state '(bodykick)))
      
      ((and (get-held :a2) (region-entered-from-center :max-butterfly (/ (* pi 2) 8)
				:min-axis-dist *trigger-radius*))
       (set-buffered-state '(spinning-hook-kick)))
      
      ((and (get-held :defense) (get-held :a1))
       (set-buffered-state '(grab)))
      
      ((and (get-tapped :defense) (get-held :down))
       (set-buffered-state '(duck)))
      
      ((get-held :defense)
       (set-buffered-state '(high-block)))

      ((and (get-held :dodge) (get-tapped :up))
       (set-buffered-state '(sidestep)))
      
      ;; ((and (get-held :dodge) (or (get-tapped :r-right) (get-tapped :r-left)))
      ;; 		 (setf buffered-state 'high-dodge :dir dir :pressure (cond ((get-held :down) 0.2) ((get-held :up) 1.0) (t 0.6))
      ;; 				  ;; (get-numeric :throttle)
      ;; 				  ))
      
      ((and (or (get-held :r-right) (get-held :r-left))  (not (get-held :a1)) (not (get-held :a2)))
       (if (not (get-held :cancel))
	   (set-buffered-state `(high-stride :dir ,dir))
	 (set-buffered-state `(running
				:vel ,(* dir 0.15) 
				:foot-pos ,(+ *neutral-leg-space* 4.0)))))
      )))

;;Idle

(defstate "idle"
  :supers
  (stunnable)
  
  :slots
  ((tpos :initform 0)
   (prone-time :initform 0)
   (anim-list :initform '("stride" "head-stun")))

  :hb
  multi-hitbox

  :animation
  combined-multi-animation

  :main-action
  ((flet ((get-untapped (input)
			(if (= (tpos state) 1)
			    (get-tapped input :released t)
			  (get-released input))))
     
     (let ((dir (if (get-held (direction-symbol)) positive negative)))
       ;;Accepts input
       (common-transitions)
       (let ((km (get-captured-keymap))
	     (bs (get-buffered-state)))
	 ;; (format t "~&*-*~a*-*~&" bs)
	 (if (= 0 (stun-recovery state))
	     (progn
	       (if bs
		   (change-state *fighter* (apply #'make-instance bs))))
	  ;; (cond
	  ;; 	    (bs
	  ;; 	     (switch-to-state (car bs)))
		
	  ;; 	    ((or (and (get-tapped :a1) (get-held :up km)))
	  ;; 	     (switch-to-state jab))

	  ;; 	    ((or (and (get-tapped :a1) (get-held (direction-symbol) km)) (and (get-tapped (direction-symbol)) (get-held :a1 km)))
	  ;; 	     (switch-to-state Kpunch))

	  ;; 	    ((and (get-held :a2) (get-held :feint))
	  ;; 	     (switch-to-state kick-feint))

	  ;; 	    ((and (get-held :a2) (get-tapped (opposite-symbol)))
	  ;; 	     (switch-to-state forward-hook-kick))

	  ;; 	    ((and (get-held :a2) (get-tapped (direction-symbol)) (get-held :up))
	  ;; 	     (switch-to-state sidekickS))

	  ;; 	    ((and (get-held :a2) (get-tapped (direction-symbol)) (get-held :down))
	  ;; 	     (switch-to-state sidekickW))

	  ;; 	    ((and (get-held :a2) (get-tapped (direction-symbol)))
	  ;; 	     (switch-to-state bodykick))

	  ;; 	    ((or (and (get-held :a2) (get-tapped :up)) (and (get-tapped :a2) (get-held :up)))
	  ;; 	     (switch-to-state roundhouse))

	  ;; 	    ((and (get-held :a2) (get-tapped :a1))
	  ;; 	     (switch-to-state bodykick))

	  ;; 	    ((and (get-held :a2) (get-held :down))
	  ;; 	     (switch-to-state spinning-hook-kick))

	  ;; 	    ((and (get-held :defense) (get-held :a1))
	  ;; 	     (switch-to-state grab))

	  ;; 	    ((and (get-tapped :defense) (get-held :down))
	  ;; 	     (switch-to-state duck))

	  ;; 	    ((get-held :defense)
	  ;; 	     (switch-to-state high-block))

	  ;; 	    ((and (get-tapped :dodge) (get-held :up))
	  ;; 	     (switch-to-state sidestep))

	  ;; 	    ;; ((and (get-held :dodge) (or (get-tapped :r-right) (get-tapped :r-left)))
	  ;; 	    ;; 		 (switch-to-state high-dodge :dir dir :pressure (cond ((get-held :down) 0.2) ((get-held :up) 1.0) (t 0.6))
	  ;; 	    ;; 				  ;; (get-numeric :throttle)
	  ;; 	    ;; 				  ))
      
	  ;; 	    ((not (get-held :h-neutral))
	  ;; 	     (if (not (get-held :cancel))
	  ;; 		 (switch-to-state high-stride :dir dir)
	  ;; 	       (switch-to-state running
	  ;; 				:vel (* dir 0.15) 
	  ;; 				:foot-pos (+ *neutral-leg-space* 4.0)))))
	  (progn
	    (decf (stun-recovery state))
	    (when (not (get-held :h-neutral))
	      ;;WARNING: CODE DUPLICATION
	      (if (not (get-held :cancel))
		  (switch-to-state high-stride :dir dir)
		(switch-to-state running
				 :vel (* dir 0.15) 
				 :foot-pos (+ *neutral-leg-space* 4.0))))))
       
	(set-buffered-state nil)))))

  :entryfunc
  (default-hitboxes)

  :rest
  (progn
    (def-statemeth animate ()
      (ccnm)
      (let ((stride (cdr (get-anim "stride")))
	    (head-stun (cdr (get-anim "head-stun"))))
	(set-time-position stride (/ 8.0 60.0))
	(set-weight stride 1.0)
	(if (> stun-recovery 0)
	    (progn
	      ;; (set-enabled head-stun t)
	      (set-time-position head-stun (/ (* 25.0 (/ (- initial-stun-recovery stun-recovery -1.0) initial-stun-recovery)) 60.0))
	      (set-weight head-stun 1.0)
	      ;; (set-weight stride 0.0)
	      )
	  (progn
	    ;; (set-enabled head-stun nil)
	    (set-weight head-stun 0.0)
	    ;; (set-weight stride 1.0)
	    ))))
  
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
     (switch-to-state jab))
     
    ((get-held :h-neutral)
     (switch-to-state idle))

    ((or
      (and (get-pressed :dodge) (get-held (opposite-symbol)) (get-held :up))
      (and (get-held :dodge)
	   (or (and (get-pressed (opposite-symbol)) (get-pressed :up))
	       (and (get-pressed (opposite-symbol)) (get-held :up))
	       (and (get-held (opposite-symbol)) (get-pressed :up)))))
     (switch-to-state backflip))

    ((and (get-held :dodge) (or (get-held :right) (get-held :left)))
     (switch-to-state stride :direction (if (get-held (direction-symbol)) positive negative)))
     
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
     (move-forward 
       (move-forward (/ (+ prev-speed kb-speed) -2.0))
       (target parent)))

   (common-transitions)

   ;; (when (get-released :a1)
;;        (set-untapped :a1))
;;      (when (get-released :a2)
;;        (set-untapped :a2))
;;      (when (get-pressed :defense)
;;        (set-tapped nil))
   
   (when (<= duration 0)
     (if (<= kb-speed 0.0)
	 (switch-to-state idle :key-buffer key-buffer)
       (switch-to-state running :key-buffer key-buffer
			:vel (- kb-speed)
			:foot-pos *neutral-leg-space*)))))


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
		   (0.0 (* 0.7 (height fighter)) 1.5 0.3))

  :main-action
  ((format t "~&~a" (get-butterfly-angle))
   (if escape-time
       (progn
	 (decf escape-time)
	 (when (<= escape-time 0)
	   (funcall escape-func)))
     (when (> tpos 6)
       (cond
	((not (get-held :defense))
	 (setf escape-time 4)
	 (setf escape-func (λ (switch-to-state idle :key-buffer key-buffer))))
	
	((get-held :a1)
	 (setf escape-time 3)
	 (setf escape-func (λ (switch-to-state grab))))
	
	((get-held :down)
	 (switch-to-state duck)
	 ;; (setf escape-time 2)
	 ;; 	 (setf escape-func (λ (switch-to-state duck)))
	 )

	((get-held (direction-symbol))
	 (move-forward 0.2))

	((get-held (opposite-symbol))
	 (move-forward -0.2)))))))


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
     ((time-pos time-pos) (duration duration) (kb-speed kb-speed) (parent parent)
      (kb-direction kb-direction) (decceleration decceleration) (key-buffer key-buffer)) high-block-stun
     (incf time-pos)
     (decf duration)
     (let ((prev-speed kb-speed)
	   (intended-poition nil))
      (decf-to kb-speed 0.0 decceleration)
      (move-forward 
       (move-forward (/ (+ prev-speed kb-speed) -2.0))
       (target parent)))

     (common-transitions)

     ;; (when (get-tapped :a1)
;;        (set-tapped :a1))
;;      (when (get-tapped :a2)
;;        (set-tapped :a2))
;;      (when (get-tapped :down)
;;        (set-tapped :down))
;;      (when (get-pressed :defense)
;;        (set-tapped nil))
      
     (cond
      ((<= duration 0)
       ;;Add key-buffer transition to idle
       (if (<= kb-speed 0.0)
	(if (and (get-held :defense) (get-tapped nil))
	    (switch-to-state high-block :key-buffer key-buffer)
	  (switch-to-state idle :key-buffer key-buffer))
	(switch-to-state running :key-buffer key-buffer
			 :vel (- kb-speed)
			 :foot-pos *neutral-leg-space*))))))


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

#|
Stride action specifications

push-off: direction (positive negative) force (advance resist) exp-stopping-pos (t nil)
glide:
contracting:
stopped: time 'int' foot (front rear)
|#

(defstate "stride"
  
  :slots
  ((vel :initform min-speed) ;Denotes velocity from the co-ordinate system where the direction being faced is positive.
   (leg-space :initform neutral-leg-space)
   (foot-height :initform 0.0)
   (body-depth :initform 0.0)
   (action :initform (list :sub :push-off
			   :direction positive
			   :force :advance))
   ;; (key-buffer :initform nil)
   ;;Debug variables
   (accel-d :initform nil))

  :constants
  ((min-speed 0.25)
   (min-dash-speed 0.15)
   (dash-accel 0.03)
   (max-dash-leg-space 29.0)
   (max-base-speed 1.5)
   (max-leg-space *wide-leg-space*)
   (neutral-leg-space *neutral-leg-space*)
   ;(max-glide-leg-space 12.0) ;Not currently used
   (min-leg-space 1.0)
   (max-accel 0.08)
   (max-deccel (* max-accel 1.0))
   (max-expanding-deccel (* max-deccel 0.1))
   (glide-deccel 0.02)
   (glide-deccel-threshold 0.6)
   (max-stoppage 1.0)
   (max-foot-height 3.0))

  :animation
  single-animation

  :main-action
  ((let ((time-window 1.0)
	 (abs-speed (abs vel))
	 (dir (if (equal vel 0.0)
		  (getf action :direction)
		(signum vel))))
     
     ;;Going 'into the grain' of motion.
     (labels ((current-dir-held ()
				(or (and (> vel 0.0) (get-held (direction-symbol))) (and (< vel 0.0) (get-held (opposite-symbol)))))

	      ;; (get-tapped (input)
;; 			  (or (equal input key-buffer) (get-pressed input)))

	      (clear-buffer ()
 			    (reset-key-buffer key-buffer))

	      (time-to-stopped ()
			       (ceiling (/ (abs vel) max-deccel)))
	      
	      (contracting-possible () ;True if the minimun distance traveled is less the the distance to the neutral-leg-space
				    ;; (stopping-possible-p vel max-deccel (- leg-space neutral-leg-space))
				    (slowing-possible-p vel #'(lambda (v fp) max-deccel) leg-space neutral-leg-space min-speed))

	      (expanding-stopping-possible () ;True if the minimum distance traveled is less the the distance to the neutral-leg-space
					   (stopping-possible-p vel max-expanding-deccel (- neutral-leg-space leg-space)))

	      (glide-leg-pos (&optional (leg-pos leg-space) (neutral-pos neutral-leg-space))
			     ;; (+ (* 0.5 (- leg-pos neutral-pos)) neutral-pos)
			     leg-pos)

	      (enter-glide ()
			   (setf leg-space (glide-leg-pos max-leg-space))
			   (setf action (create-glide-form leg-space)))

	      (goto-neutral ()
			    (switch-to-state idle))

	      (high-deccel (vel foot-pos)
			   0.2)

	      (glide-advancing-accel (vel foot-pos dir)
				     (let ((spd (abs vel)))
				       (* dir
					(cond
					 ((< spd 1.0) 0.01)
					 ((< spd 1.4) 0.0)
					 (t -0.01)))))
	      
	      (glide-resisting-accel (vel foot-pos dir)
				     (let ((spd (abs vel)))
				      (* dir
				       (cond
					((< spd 0.6) 0.02)
					((< spd 1.2) -0.01)
					(t -0.03)))))
	      
	      (push-off ()
			(let ((dir (if (equal vel 0.0)
				       (getf action :direction)
				     (signum vel))))
			  (if (equal (getf action :force) :advance)
			      ;;When action = :advance
			      (let ((accel (* dir (let ((ne (* (/ (- max-base-speed (abs vel)) max-base-speed) max-accel)))
						    (if (> ne 0.0) ne 0.0)))))
				(format t "In advancing.~&")

				(let* ((candidate-leg-pos (+ leg-space abs-speed))
				       (new-leg-pos (if (< candidate-leg-pos max-leg-space)
							candidate-leg-pos
						      max-leg-space))
				       (leg-delta (- new-leg-pos leg-space)))
				  (incf leg-space leg-delta)
				  (move-forward (* leg-delta dir)))

				(when (>= leg-space neutral-leg-space)
				  (appending-cond
				   (push-common-transitions)
				       
				   (((get-tapped :a1)
				     (format t "Attempting straight punch.~&")
				     (when (and (contracting-possible)
						(< (abs vel) 3.0))
				       (clear-buffer)
				       (switch-to-state straight
							:forward-speed (abs vel)
							:leg-space leg-space))))))
				
				(incf vel accel)
				(if (> (abs vel) max-base-speed)
				    (setf vel (* dir max-base-speed))))
			    
			    ;;When action = :resist
			    (let ((deccel (* dir max-expanding-deccel)))
			      (if (< leg-space neutral-leg-space)
				  (progn
				    (format t "In close expanding resist.~&")
				    (if (getf action :exp-stopping-pos)
					;;If it is possible to stop within the alloted distance.
					;;NOTE: Change this to THE FORMULA later on.
					(progn
					  (format t "In expaning slowing SP.~&")
					  ;;Constant velocity
					  (incf leg-space abs-speed)
					  (move-forward vel)

					  (incf vel (glide-resisting-accel vel leg-space dir))
					  
					  (when (>= leg-space neutral-leg-space)
					    (setf action (list :sub :stopped :time 4 :foot dir))
					    (setf vel 0.0)))
				      
				      ;;If it is not possible to stop before reaching the neutral leg position.
				      (progn
					(format t "In expanding slowing SI.~&")
					
					(incf leg-space abs-speed)
					(move-forward vel)
					
					(decf vel deccel))))
				
				;;When resisting, but 
				(progn
				  (format t "In wide expanding resist.~&")
				  
				  (let* ((candidate-leg-pos (+ leg-space abs-speed))
					 (new-leg-pos (if (< candidate-leg-pos max-leg-space)
							  candidate-leg-pos
							max-leg-space))
					 (leg-delta (- new-leg-pos leg-space)))
				    (incf leg-space leg-delta)
				    (move-forward (* leg-delta dir)))
				  
				  (appending-cond
				   (push-common-transitions))
				  
				  (decf vel deccel)))))))
	      
	      (glide (time)
		     (let ((deccel (* glide-deccel (signum vel))))
					 
		       (format t "In glide.~&")
					 
		       (decf leg-space (abs vel))
		       (move-forward vel)

		       (if (current-dir-held)
			   (incf vel (glide-advancing-accel vel leg-space dir))
			 (incf vel (glide-resisting-accel vel leg-space dir)))
					  
		       (if (<= leg-space min-leg-space)
			   (let ((neg-time (/ (- min-leg-space leg-space) (abs vel) time)))
			     (format t "Min leg pos reached.~&")
			     (setf leg-space min-leg-space)
			     (setf action
				   (list :sub :push-off
					 :direction dir
					 :force (if (current-dir-held) :advance :resist)
					 :exp-stopping-pos (expanding-stopping-possible)))))))
	      
	      (slowing ()
		       (format t "In wide slowing.~&")
			(let* ((direction (signum vel)) ;;forward or backward.
			       (deccel (* max-deccel direction)))
			  (setf action (list :sub :contracting))
			  
			  (decf leg-space (abs vel))
			  
			  (when (< leg-space min-leg-space)
			    (format t "TRIP!!!!.~&")
			    (setf leg-space min-leg-space)
			    (setf action (list :sub :push-off
					       :direction direction
					       :force :resist))) ;;NOTE: Change this to a stagger state later on.
			   
			  (if (or
				 (< (abs vel) min-speed)
				 (not (equal direction (signum vel))))
			    (progn
			      (setf vel 0.0)
			      (goto-neutral))
			   
			    (progn
			     (move-forward vel)
			     (decf vel deccel)))))
	      
	      (stopped ()
		       (format t "In stopped.~&")
		       ;;ADD STUFF HERE!
		       (decf (getf action :time))
		       (when (<= (getf action :time) 0)
			 (goto-neutral)))

	      (stoppage ()
			(format t "In stoppage.~&")
			(let ((tinit (getf action :tinit)))
			 (cond
			  
			  ((equal (getf action :phase) :hopping)
			   (incf leg-space (abs vel))
			   (move-forward vel)
			   (incf vel (* (getf action :initial-accel) dir))
			   (if (> tinit 0)
			       (decf tinit)
			     (progn
			       (setf tinit (getf action :air-time))
			       (setf (getf action :phase) :in-air))))

			  ((equal (getf action :phase) :in-air)
			   ;; (decf leg-space (abs vel))
			   (move-forward vel)
			   (if (> tinit 0)
			       (decf tinit)
			     (setf (getf action :phase) :resisting)))

			  ((equal (getf action :phase) :resisting)
			   (let* ((deccel (* (getf action :deccel) dir))
				  (new-vel (- vel deccel)))
			    (decf leg-space (abs vel))
			    (move-forward vel)
			    
			    (if (equal (signum vel) (signum (- new-vel min-speed)))
				(setf vel new-vel)
			      (progn
				(setf action (list :sub :stopped :time (getf action :stopping-time) :foot dir))
				(setf vel 0.0))))))

			 (setf (getf action :tinit) tinit)))

	      (high-dash ()
			 (let* ((expanding (getf action :expanding))
				(leg-delta (* abs-speed expanding))
				(max-vel 1.9)
				(dash-accel (* dir (* (/ (- max-vel (abs vel)) max-vel) 0.18))))

			   (when (equal expanding positive)
			    (incf leg-space leg-delta)
			    (move-forward (* (abs leg-delta) dir))
			    (incf vel (* dash-accel expanding))

			    (when (get-pressed :a1)
			      (switch-to-state straight
					       :forward-speed vel
					       :leg-space leg-space)))
			   
			   (when (and (equal expanding positive)
				      (or (not (current-dir-held))
					  ;; (get-held :down)
					  ;; (>= leg-space max-dash-leg-space)
					  ))
			       (setf expanding negative))

			   (when (and (equal expanding positive)
				      (>= leg-space max-dash-leg-space))
			       (switch-to-state running
						:vel vel
						:foot-pos leg-space))

			   (when (equal expanding negative)
			     (incf vel (* dash-accel expanding))
			     (incf leg-space leg-delta)
			     (move-forward (* (abs leg-delta) dir)))

			   (when (and (equal expanding negative)
				      (< abs-speed min-dash-speed))
			     (goto-neutral))

			   (setf (getf action :expanding) expanding))))
       (cond
	((equal (getf action :sub) :push-off)
	 (push-off))
	((equal (getf action :sub) :glide)
	 (glide 1.0))
	((equal (getf action :sub) :contracting)
	 (slowing))
	((equal (getf action :sub) :stopped)
	 (stopped))
	((equal (getf action :sub) :stoppage)
	 (stoppage))
	((equal (getf action :sub) :high-dash)
	 (high-dash)))

       (when (get-pressed :a1)
	 (set-tapped :a1))

       (format t "vel: ~a ~20Tleg: ~a~&action: ~a" vel leg-space action)
       (format t "~&f: ~a d: ~a cp: ~a tpos: ~a~&" (current-dir-held) (get-held :down) (contracting-possible) tpos)
       (format t "**********************************~&"))))

  ;; :initfunc
;;   ((&key direction)
;;    ((let ((direction (if direction direction positive)))
;;       ())))
  
  :rest
  (progn
    (defmethod animate ((sa stride))
      (let ((ls (/ (float (leg-space sa)) 60.0))
	    (ani (animation sa)))
	(set-time-position ani ls)))

    (defmethod initialize-instance :after ((state stride) &key direction action-form)
      
      (setf (getf (action state) :direction) direction)
      (when action-form
	(case action-form
	  (:high-dash
	   (setf (vel state) min-dash-speed)
	   (setf (action state) (list :sub :high-dash :expanding positive)))))

      (setf (vel state) (* direction (abs (vel state)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; high-stride

(defconstant *max-stride-vel* 1.9)
(defconstant *max-stride-accel* 0.4)
(defconstant *max-hdash-spd* 1.8)
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
  :supers
  (stunnable)

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
   (free-lead :initform t);;True if the front foot can be used freely, allowing attacks to be preformed out of the stride.
   (goal-action :initform nil) ;;E.g. If a punch input is recieved,
   ;;but punch cannot yet be preformed, goal-action will become punch to signal the transition.
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
    (default-hitboxes)
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
	  (to-negative (or glide-asap (not current-dir-held) ;; (get-held :defense)
			   )))

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
	     ((or (and to-negative expanding-possible) (>= foot-pos (max-foot-pos-per-speed vel)))
	      (format t "TN1 cdh:~a" current-dir-held)
	      (set-expanding-negative))
	     
	     ;; ((not free-lead)
;; 	      ())
	     
	     ((and (get-tapped :a1) (equal 0 (stun-recovery state)))
	      (set-tapped :jab)
	      (switch-to-state glide-in
			     :vel (if (if (>= dir 0.0)
					  (<= (- vel min-speed) 0.0)
					(>= (+ vel min-speed) 0.0))
				      (* dir min-speed)
				    vel)
			     :foot-pos foot-pos
			     :key-buffer key-buffer))

	     ((and (get-tapped :a2) (get-held (direction-symbol)) (equal 0 (stun-recovery state)))
	      (let ((foot-value (/  (- foot-pos *neutral-leg-space*) (- *wide-leg-space* *neutral-leg-space*))))
		(format t "~&*************************f-v: ~a**~& " foot-value)
	       (if (and (get-held :feint) (< foot-value 0.8))
		   (switch-to-state kick-feint)
		(cond
		 ((and (get-held :up) (< foot-value 0.2))
		  (switch-to-state sidekickS))
		 ((and (not (get-held :down)) (< foot-value 0.7))
		  (switch-to-state sidekickM))
		 (t
		  (switch-to-state sidekickW
				   :extra-time (round (* 7.0 foot-value))))))))

	     ;; ((get-tapped :dodge)
;; 	      (if (and (get-held forward-symbol)) 
;; 		  (switch-to-state high-dodge
;; 				   :dir dir
;; 				   :intspeed (abs vel)
;; 				   :pressure (cond ((get-held :down) 0.2) ((get-held :up) 1.0) (t 0.6))
;; 				   ;; (get-numeric :throttle)
;; 				   )
;; 		(set-tapped :dodge)))

	     ((or (get-tapped :cancel) glide-asap)
	      (format t "CA1")
	      (if expanding-possible
		  (switch-to-state glide-in
				   :vel (if (if (>= dir 0.0)
						(<= (- vel min-speed) 0.0)
					      (>= (+ vel min-speed) 0.0))
					    (* dir min-speed)
					  vel)
				   :foot-pos foot-pos)
		(setf glide-asap t)))

	     ((equal goal-action :jab)
	      (if (get-held :down)
		      (switch-to-state forward-jab
				       :vel vel
				       :foot-pos foot-pos)
		    (switch-to-state straight
				     :forward-speed vel
				     :leg-space foot-pos)))
	     ((and (get-held :down) (get-held :defense))
	      (switch-to-state step-to-duck
			       :vel vel
			       :foot-pos foot-pos))))

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
	   ((or (and (get-tapped forward-symbol) (not (get-held :a2))) (get-tapped :cancel))
	    (format t "~&******* TO GLIDE-IN *******~&")
	    (switch-to-state glide-in
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

	   ((get-pressed :a1)
	     (set-tapped :a1)
	     (switch-to-state glide-in
			     :vel (if (if (>= dir 0.0)
					  (<= (- vel min-speed) 0.0)
					(>= (+ vel min-speed) 0.0))
				      (* dir min-speed)
				    vel)
			     :foot-pos foot-pos
			     :key-buffer key-buffer))

	   ((and (get-held :a2) (get-pressed (direction-symbol)))
	    (set-tapped (direction-symbol)))

	   ;; ((get-pressed :dodge)
;; 	    (if (get-held forward-symbol)
;; 	     (switch-to-state high-dodge
;; 			      :dir dir
;; 			      :intspeed (abs vel)
;; 			      :pressure (cond ((get-held :down) 0.2) ((get-held :up) 1.0) (t 0.6))
;; 			      :key-buffer key-buffer)
;; 	     (set-tapped :dodge)))

	   ((and (get-held :down) (get-held :defense))
	      (switch-to-state step-to-duck
			       :vel vel
			       :foot-pos foot-pos))
	  
	   ((<= spd min-speed)
	    (format t "I1")
	    (switch-to-state idle
			     :key-buffer key-buffer))))))

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
   (pressure)
   (starting :initform t)
   (stopping-time :initform 0)
   (air-time :initform 0)
   (start-speed :initform 0.15)
   (air-time-ended :initform nil)
   (stopped-time))

  :animation
  single-animation

  :alt-name
  "stride"

  :funcs
  ((vel (* dir spd))
   (forward-symbol (if (>= dir 0.0) (direction-symbol) (opposite-symbol)))
   (target-weight (get-numeric forward-symbol))
   (max-start-time (+ 2 (round (* pressure 4.0))))
   (max-speed (+ (* pressure 1.4) 1.5))
   (accel (- 0.6 (* pressure 0.2))))

  :main-action
  ((cond
    ;; ((get-pressed :dodge)
;;      (switch-to-state running
;; 		      :vel vel
;; 		      :foot-pos foot-pos))

    ((and (get-held :dodge) (get-tapped forward-symbol))
     (switch-to-state high-dodge :dir dir :spd spd :pressure (cond ((get-held :down) 0.2) ((get-held :up) 1.0) (t 0.6))))
    
    (starting
     (when (get-held :down) (setf pressure 0.2))
     (when (get-held :up) (setf pressure 1.0))
     (incf air-time)
     (incf-to spd max-speed accel)
     (incf foot-pos spd)
     (move-forward vel)
     (when (or (>= air-time max-start-time))
       (let ((new-air-time (round (* 0.7 air-time))))
	 (setf spd (+ spd 0.2 (* air-time 0.01)))
	 (setf stopping-time (round (* 3.0 new-air-time)))
	 (setf air-time new-air-time)
	 (setf starting nil)
	 (if (and (get-held :cancel))
	     (switch-to-state running
			    :vel vel
			    :foot-pos foot-pos
			    :key-buffer key-buffer)))))

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
     (decf stopped-time)
     (setf spd 0.0)
     (tapped-list :r-right :r-left)
     (when (<= stopped-time 0)
      (switch-to-state idle :key-buffer key-buffer))))

   (format t "~&tpos: ~a spd: ~a ~20Tleg: ~a~& air-time ~a ~20Tstopping-time: ~a" tpos spd foot-pos air-time stopping-time))

  :initfunc
  ((&key (intspeed 0.15))
   ((format t "in")
    (setf (slot-value state 'spd) intspeed)
    (setf (slot-value state 'start-speed) intspeed)
    (setf (slot-value state 'stopped-time)  (round (* 4.0 (pressure state))))
    (format t "out")))

  :rest
  (progn
    (def-statemeth animate ()
      (set-time-position animation (/ foot-pos 60.0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; running

;;Returns the absolute value of the stick's "wing strength".
(defun get-speed-input ()
  (let* ((butterfly (get-butterfly-angle))
	 (butterfly (if butterfly butterfly 0.0)))
    (format t "bf: ~a" butterfly)
    (* (get-axis-dist) (if (> butterfly (/ PI 2.0))
			   1.0
			 (/ butterfly (/ PI 2.0))))))

(defstate "running"

  :supers
  (component-velocity stunnable)

  :animation
  multi-animation

  :constants
  ((max-speed 2.2)
   (max-accel 0.08)
   (max-running-weight 8)
   (burst-recovery-limit 10.0)
   (min-running-weight 1))

  :slots
  ((foot-pos)
   (running-weight :initform min-running-weight)	;;The amount of time it would take to return to a stride.
   (weight-limit-met :initform nil)
   (time-till-return :initform 0)
   (aux-state :initform nil)
   (returning-state :initform nil)
   (burst-recovery :initform 0)
   (recent-target-speed :initform 0.0)
   (recent-target-diffrence :initform 0.0);;The 
   (animation-incr :initform 0.2)
   ;;(stun-recovery :initform 0)
   (anim-list :initform (list :wc  (make-instance 'single-animation :name "walkcycle")
			      :stride (make-instance 'single-animation :name "stride"))))
  
  :main-action
  ((let* ((forward-symbol (if (> dir 0.0) (direction-symbol) (opposite-symbol)))
	  (running-weight-limit (round (+ min-running-weight (* (- max-running-weight min-running-weight) (/ spd max-speed)))))
	  (dir-held (or (and (> dir 0.0) (get-held (direction-symbol))) (and (< dir 0.0) (get-held (opposite-symbol)))))
	  (vel (* spd dir))
	  (attempt-return (or (not dir-held) (get-held :defense)))
	  (accel max-accel)
	  (forward-weight (* dir (get-direction) (if (get-held :left) (- (get-speed-input)) (get-speed-input)))) ;;Relative to current movement direction
	  (target-speed (* max-speed forward-weight))
	  (target-diffrence (- target-speed spd)))

     (labels ( ;; (get-tapped (input &optional un)
	      ;; 			  (or (equal input key-buffer) (get-pressed input)))
	      ;; (set-tapped (input &optional un)
	      ;; 			  (let ((info (if un input `(:un ,input))))
	      ;; 			    (setf keybuffer info)))
	      (speed-to-target (target-spd &optional (accel max-accel))
			       (if (<= spd target-spd)
				   (incf-to spd target-spd accel)
				 (decf-to spd target-spd accel)))
	      (in-instep ()
			 (let ((scaled-target-weight (* 0.3 (log (+ (abs recent-target-diffrence) 1.0) 2.3))))
			   (if (> target-diffrence recent-target-diffrence)
			       (progn
				 (setf aux-state :in-step)
				 (set-current-animation :stride)
				 (setf recent-target-speed target-speed)
				 (setf recent-target-diffrence target-diffrence)
				 (setf burst-recovery (+ 1 (round (* burst-recovery-limit (/ recent-target-diffrence max-speed))))))
			     (if (<= (- recent-target-speed spd) 0.0)
			       (progn;;When the current speed has met/exceeded the target (and the target diff has not been exceeded).
				 (decf burst-recovery)
				 (when (<= burst-recovery 0)
				   (setf aux-state nil)
				   (set-current-animation :wc)))
			       ;;When the current speed has YET TO met/exceeded the target (and the target diff has not been exceeded).
			       (when (and attempt-return ;; (<= spd (- (+ recent-target-speed (* recent-target-diffrence 1.0)) recent-target-diffrence))
					  (<= 0 burst-recovery))
				 (setf aux-state :close-step)
				 (set-current-animation :wc))))
			   (speed-to-target recent-target-speed scaled-target-weight)))
	      (in-closestep ()
			    (let ((scaled-target-weight (* 0.15 (log (+ (abs recent-target-diffrence) 1.0) 2.2))))
			      (speed-to-target 0.0 scaled-target-weight)
			      (when (equal 0.0 spd)
				(setf aux-state nil)
				(set-current-animation :wc))))
	      (enter-return ()
			    (setf returning-state nil)
			    (setf time-till-return running-weight))
	      (enter-neutral ()
			     (setf aux-state nil)
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
	 
       (when (and (>= dir 0) (get-pressed :a2))
	 (set-tapped :a2)
	 (enter-sidekick))
	 
       ;; (when (get-pressed :dodge)
;; 	 (set-tapped :dodge)
;; 	 (enter-straight))
	 
       (when (get-pressed :dash)
	 ;; (set-tapped :cancel)
	 (enter-glide-in))
       
       (move-forward vel)
       
       (format t "~&**********************************~&")
       
       (cond
	((and (not aux-state) (not returning-state))
	 (if (not attempt-return)
	     (progn
	       (incf-to running-weight running-weight-limit 1 :force-to-limit t)
	       ;;Speed increases should only occur when the appropriate weight limit is reached.
	       (when (>= running-weight running-weight-limit)
		 (setf weight-limit-met t))
	       (when weight-limit-met
		 (if (and (< target-diffrence 0.3) (equal burst-recovery 0))
		     (progn
		       (speed-to-target target-speed)
		       (setf recent-target-diffrence 0.0))
		   (in-instep))))
	   (let ((accel max-accel))
		
	     (decf-to spd 0.1 accel)
	     (decf-to time-till-return 0)
	     (when (and (= time-till-return 0) (<= spd 0.1))
	       ;; (switch-to-state high-stride :vel (* spd dir) :foot-pos foot-pos :expanding negative)
	       (switch-to-state idle :key-buffer key-buffer)))))

	((equal aux-state :in-step)
	 (in-instep))

	((equal aux-state :close-step)
	 (in-closestep))
	
	((equal returning-state :to-straight)
	 (speed-to-target 1.6)
	 (when (equal 0 (stun-recovery state))
	     (decf-to time-till-return 0)
	     (when (= time-till-return 0)
	       ;; (switch-to-state straight :forward-speed (* spd dir) :leg-space foot-pos)
	       (switch-to-state high-stride :vel (* spd dir) :foot-pos foot-pos :key-buffer key-buffer))))

	((equal returning-state :to-grab)
	 (speed-to-target 0.0)
	 (when (equal 0 (stun-recovery state))
	   (decf-to time-till-return 0)
	   (when (and (= time-till-return 0) (<= spd 0.0))
	     (switch-to-state grab :key-buffer key-buffer))))

	((equal returning-state :to-frontkick)
	 (speed-to-target 0.2)
	 (when (equal 0 (stun-recovery state))
	   (decf-to time-till-return 0)
	   (when (and (= time-till-return 0) (<= spd 0.2))
	     ;; (switch-to-state straight :forward-speed (* spd dir) :leg-space foot-pos)
	     (switch-to-state frontkick :vel (* spd dir) :foot-pos (+ 4.0 *neutral-leg-space*) :key-buffer key-buffer))))

	((equal returning-state :to-sidekick)
	 (speed-to-target 1.2)
	 (when (equal 0 (stun-recovery state))
	   (decf-to time-till-return 0)
	   (when (= time-till-return 0)
	     (if (get-in-region :min-butterfly (/ (* PI 5) 8) :min-axis-dist *trigger-radius*)
		 (switch-to-state sidekickS)
	      (if (get-in-region :min-butterfly (/ (* PI 3) 8) :min-axis-dist *trigger-radius*)
		  (switch-to-state sidekickM)
	       (switch-to-state sidekickW))))))

	((equal returning-state :to-glide-in)
	 (speed-to-target 1.6)
	 (decf-to time-till-return 0)
	 (when (= time-till-return 0)
	   (switch-to-state glide-in :vel (* spd dir) :foot-pos (+ 5.0 *neutral-leg-space*)
			    :stun-recovery (stun-recovery state)))))

       (setf (animation-incr (get-anim :wc)) (* vel 0.5))
       
       (format t "~&Target-Speed: ~a Forward-Weight ~a Forward-Symbol ~a~&
target-diffrence: ~a burst-recovery: ~a~&recent-target-diffrence~a~&" target-speed forward-weight forward-symbol target-diffrence burst-recovery recent-target-diffrence)
       (format t "vel: ~a ~20Tleg: ~a~& running-weight: ~a ~20Tweight-limit-met: ~a~&r-state ~a~& a-state: ~a ~&ttr: ~a ~10Tdir: ~a ts: ~a~&"
	       vel foot-pos running-weight weight-limit-met returning-state aux-state time-till-return dir (- (+ recent-target-speed (* recent-target-diffrence 0.5)) recent-target-diffrence))))))

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

   (common-transitions)

   ;; (when (get-pressed :a1)
;;      (set-tapped :a1))

;;    (when (get-pressed :a2)
;;      (set-tapped :a2))

;;    (when (get-pressed :dodge)
;; 	 (set-tapped :dodge))
   
;;    (when (get-pressed :cancel)
;;      (set-tapped :cancel))
   (format t "~&**************************~&")
   (let ((min-spd (* 1.0 (/ (- *neutral-leg-space* min-foot-pos) *neutral-leg-space*))))
     (format t "~&** Min-Spd ~a  key-label: ~a status: ~a **~&" min-spd (key-label key-buffer) status)
    (if (> vel 0.0)
	(incf-to vel (max min-spd (min 1.4 target-speed)) 0.06)
      (decf-to vel (min (- min-spd) (max -1.4 (- target-speed))) 0.06)))

   (when (equal status :hori)
    (decf foot-pos (abs vel))

    (when (<= foot-pos min-foot-pos)
     (format t "Min leg pos reached.~&")
     (setf foot-pos min-foot-pos)
     (if (or t (not (get-held :down)) (key-label key-buffer))
	 (let ((sval (round (+ 10.0 (* 5.0 (/ (abs vel) *max-stride-vel*))))))
	   (setf status sval)
	   (setf exit-height sval))
       (switch-to-state glide-out
			:vel vel
			:foot-pos foot-pos
			:entry-height exit-height
			:free-lead nil
			:max-foot-pos  (* (- *wide-leg-space* *neutral-leg-space*) (/ (abs (vel state)) *max-stride-vel*)) ;; (- (* 2 *neutral-leg-space*) foot-pos)
			:key-buffer key-buffer))))

   (when (not (equal status :hori))
     (decf-to status 0)
     (if (> vel 0.0)
	(decf-to vel (min 1.1 target-speed) 0.05)
      (incf-to vel (max -1.1 (- target-speed)) 0.05))
     (if (<= status 0)
      (if (get-held :cancel)
	  (switch-to-state running :vel vel :foot-pos *neutral-leg-space*)
	(switch-to-state glide-out
			 :vel vel
			 :foot-pos foot-pos
			 :entry-height exit-height
			 :max-foot-pos  (* (- *wide-leg-space* *neutral-leg-space*) (/ (abs (vel state)) *max-stride-vel*)) ;; (- (* 2 *neutral-leg-space*) foot-pos)
			 :key-buffer key-buffer))))

   
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
   (max-foot-pos)
   (free-lead :initform t))

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

    (common-transitions)

    ;; (when (get-pressed :a1)
;; 	 (set-tapped :a1))

;;     (when (get-pressed :a2)
;;       (set-tapped :a2))

;;     (when (get-pressed :dodge)
;; 	 (set-tapped :dodge))
    
;;    (when (get-pressed :cancel)
;;      (set-tapped :cancel))

    (when (>= foot-pos max-foot-pos) 
      (format t "Min leg pos reached.~&")
     
					;Switch to high-stride
      (if (or (get-held :a1) (get-tapped :a1))
	  (switch-to-state Kpunch)
	  (switch-to-state high-stride
			   :vel vel
			   :foot-pos foot-pos
			   :free-lead free-lead
			   :key-buffer key-buffer)))

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
  ((vel (* -1.0 spd))
   (end-time 18))

  :slots
  ((spd :initform 0.6)) ;his should be constant throughout the life of this state.

  :main-action
  ((move-forward vel)
   (common-transitions)
   (when (>= tpos end-time)
     (switch-to-state idle :key-buffer key-buffer)))

  :rest
  (progn
    (def-statemeth animate ()
      (scale-animation (list 0 end-time) (list 0 20)))
    (def-statemeth side-spd ()
      2.2)))


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
	 (switch-to-state idle)))

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

(defconstant *duck-mid-point* 7)

(defstate "duck"

  :slots
  ((to-slip :initform nil)
   (to-haymaker :initform nil))

  :funcs
  ((holding (get-held :down))
   (mid-time *duck-mid-point*)
   (end-time (* 2 *duck-mid-point*)))
  
  :main-action
  ((when (get-held (opposite-symbol))
     (setf to-slip t))
   (when (and (get-tapped :a1))
     (setf to-haymaker t)
     )
   (if (and (< tpos mid-time) (not (get-held :down)))
       (setf tpos (- end-time tpos))
     (if (= mid-time tpos)
       (cond
	(to-slip (switch-to-state ducking-slip))
	(to-haymaker (switch-to-state rising-haymaker))
	(holding (setf tpos (- mid-time 1))))
       ;; (if (and (= mid-time tpos) holding)
;; 	   (setf tpos (- mid-time 1)))
       ))

   (when (= tpos end-time)
     (switch-to-state idle :key-buffer key-buffer))
   (format t (print-state state)))

  :rest
  (progn
    (def-statemeth height ()
      (if (and (> tpos 2) (< tpos 12))
	      (- (call-next-method) 20)
	(call-next-method)))

    (def-statemeth animate ()
      (ccnm)
      (set-time-position animation (float (/ tpos 60.0))))))

(defstate "ducking-slip"

  :alt-name
  "duck"

  :funcs
  ((end-time 14))
  
  :main-action
  ((move-forward -1.3)
   (when (= tpos end-time)
     (switch-to-state idle :key-buffer key-buffer)))

  :rest
  (progn
    (def-statemeth height ()
      (if (and (> tpos 0) (< tpos 20))
	      (- (call-next-method) 20)
	(call-next-method)))

    (def-statemeth animate ()
      (ccnm)
      (set-time-position animation (float (/ (+ tpos 6.0) 60.0 2.0))))))

(defstate "step-to-duck"
  :supers
  (component-velocity)
  
  :alt-name
  "duck"
  
  :funcs
  ((vel (* spd dir))
   (end-time 15))

  :slots
  ((foot-pos)
   (c-time :initform nil))

  :main-action
  ((if (< foot-pos *wide-leg-space*)
     (progn 
       (incf-to spd 1.5 0.3)
       (incf-to foot-pos *wide-leg-space*)
       (move-forward vel))
     (progn
       (setf foot-pos *wide-leg-space*)
       (if (not c-time)
	   (setf c-time 0))
       (incf c-time)
       (format t "~a" c-time)
       (when (>= c-time end-time)
	 (switch-to-state duck :tpos (- *duck-mid-point* 1)
			  :key-buffer key-buffer))))
   (format t (print-state state)))

  :rest
  (progn
    (def-statemeth height ()
      (if (> tpos 3)
	  (- (call-next-method) 20)
	(call-next-method)))
    
    (def-statemeth animate ()
      (ccnm)
      (set-time-position animation (float (/ tpos 60.0))))))
