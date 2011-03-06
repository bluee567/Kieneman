(in-package "KIENEMAN")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; States
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric change-state (state-container new-state)
  (:documentation "
The container object must impliment the state function which must return a state object."))

(defgeneric transition-state (container old-state new-state)
  (:documentation "Changes the old state to the new state, executing any actions required to
do so."))

(defgeneric enter-state (state-container state)
  (:documentation "Accepts a container object (which will contain the state) and a state for the
container to be in. (i.e. a fighter (the container) may transition from an idle state to a walking state
by calling change-state which will call this function). This function will preform the "))

(defgeneric exit-state (state-container state)
  (:documentation ""))

(defgeneric transition-animation (container old-anim new-anim)
  (:documentation "Changes the old animation state to the new state."))

(defgeneric enter-animation (anim animation)
  (:documentation "Accepts a container object (which will contain the state) and a state for the
container to be in. (i.e. a fighter (the container) may transition from an idle state to a walking state
by calling change-state which will call this function). This function will preform the "))

(defgeneric exit-animation (anim animation)
  (:documentation ""))

(defgeneric animate (object)
  (:documentation "Manipulates the rendering system so that the character will
be drawn appropriately when the screen is rendered."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;State

(defclass+ state (rectangular-hitbox low-y-box displayed-box)
  ((:ia parent)
   (:ia radius
	:initform 1.0)
   (:ia height
	:initform 1.0)))

;;For states, enter state should be used in place of initialise-instance.

(defmethod enter-state :after ((f1 fighter) (state state))
  (with-accessors ((display display)) state
    (setf display (make-hit-rectangle (x state) (median-y state) (width state) (height state) "blue" *mgr*))))

(defmethod exit-state :after ((f1 fighter) (state state))
  (with-accessors ((display display)) state
    (destroy-entity *mgr* display)))

(defmethod x ((state state))
  (x (parent state)))

(defmethod y ((state state))
  (y (parent state)))

(defmethod radius ((state state))
  (* (radius (parent state)) (slot-value state 'radius)))

(defmethod height ((state state))
  (* (height (parent state)) (slot-value state 'height)))

(defmethod bottom ((state state))
  (y (parent state)))

(defmethod top ((state state))
  (+ (y (parent state)) (height state)))

(defmethod left ((state state))
  (- (x (parent state)) (radius state)))

(defmethod right ((state state))
  (+ (x (parent state)) (radius state)))


(defvar *fighter* nil)

(macrolet ((around-action ()
			  `(let* ((*fighter* (parent state))
				 (*keymap* (input-funcs *fighter*)))
			    (call-next-method))))
  
 (defmethod main-action :around ((state state))
  (around-action))

 (defmethod animate :around ((state state))
   (around-action)))

(defmacro defanim (name supers body &rest meta)
  `(defclass+ ,name (single-animation state ,@supers)
     ((name :initform ,(string name))
      ,@body)
     ,@meta))

(defmacro switch-to-state (state-name)
  "Switches states and executes the main body of the
state that has been switched to."
  `(progn
     (let ((state (make-instance (quote ,state-name))))
      (change-state *fighter* state)
      ;; (main-action state)
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Single Animation

(defclass animation () ())

(defclass+ single-animation (animation)
  ((:iea name)
   (:ia animation)))

(defdelegate single-animation animation
  (get-weight
   (set-weight weight)
   (add-time dtime)
   get-time-position
   (set-time-position time)
   get-enabled
   (set-enabled bool)))

(defmethod animate ((sa single-animation))
  (add-time (animation sa) (/ 1.0 60.0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Combined Animation

(defclass+ combined-animation (animation)
  ((:iea name)
   (:ia animation)
   (:ia prev-animation
	:initform nil)
   (:ia weight
	:initform 0.0)
   (:ia weight-incr
	:initform 0.1)))

(defdelegate combined-animation animation
  (get-weight
   (set-weight weight)
   (add-time dtime)
   get-time-position
   (set-time-position time)
   get-enabled
   (set-enabled bool)))

(defmethod animate ((ca combined-animation))
  (with-accessors ((anim animation)
		   (panim prev-animation)
		   (weight weight)
		   (weight-incr weight-incr)) ca
   ;(add-time anim (/ 1.0 150.0))
   
   (if (and (< weight 1.0) (not (eql panim nil)))
    (progn
     (incf weight weight-incr)
     (add-time panim (/ 1.0 60.0))
     (set-weight panim (- 1.0 weight)))
    (progn
      (setf weight 1.0)
      (when (not (eql panim nil))
	(exit-animation (parent ca) panim)
	(set-enabled anim t);If the prev-animation was the same as the current animation, this makes sure the current animation is still set.
	(setf panim nil))))
   
   (set-weight anim weight)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; "A simple linear animation that blends in the 
;; previous animation."


(defclass+ combined-linear-animation (combined-animation)
  ((:ia animation-incr
	:initform (/ 1.0 60.0))))

(defmethod animate  ((ca combined-linear-animation))
  (call-next-method)
  (add-time (animation ca) (animation-incr ca)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Idle

(defclass+ idle (combined-linear-animation state)
  ((name :initform "idle")
   (:ia tpos :initform 0)))

;; (defmethod animate ((sa single-animation))
;;   (with-accessors ((anim animation)) sa
;;    (let ((len (get-length anim))
;; 	 (pos (+ (get-time-position anim) (/ 1.0 60.0))))
;;      (if (> pos len)
;; 	 (decf pos len))
;;      (set-time-position anim (/ 1.0 60.0)))))

(defmethod main-action ((idle idle))    
  (incf (tpos idle))

  ;; (when (= (tpos idle) 1)
;;     (set-input-override (parent idle) t))
  
  (cond
   ((not (get-held :h-neutral))
    (switch-to-state walkcycle))
   
   ((get-pressed :a1)
    (switch-to-state jab))

   ((get-held :a2)
    (switch-to-state roundhouse))

   ((get-held :defense)
    (switch-to-state high-block)))

  ;; (when (= (tpos idle) 1)
;;     (set-input-override (parent idle) nil)
;;     (set-override-keys (parent idle) nil))
  )

;; (defmethod animate ((idle idle))
;;   (call-next-method)
;;   (add-time (animation idle) (/ 1.0 100.0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Walking

(defclass+ walkcycle (combined-animation state)
  ((name :initform "walkcycle")))

(defmethod main-action ((walkcycle walkcycle))
    
    (cond

     ((get-pressed :a1)
      (switch-to-state jab))
     
     ((get-held :h-neutral)
      (switch-to-state idle))     
     
     ((get-held :left) 
      (decf (x *fighter*)))

     ((get-held :right)
      (incf (x *fighter*)))))

(defmethod animate ((walkcycle walkcycle))
  (call-next-method)
  (if (get-held (direction-symbol *fighter*)) ;; (if (= right (get-direction *fighter*))
;; 					 (get-held :right)
;; 				       (get-held :left))
      (add-time (animation walkcycle) (/ 1.0 150.0))
    (add-time (animation walkcycle) (/ -1.0 150.0))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stunned

(defclass+ stunned (combined-linear-animation state)
  ((name :initform "stunned")
   (:iea duration)))

(defmethod main-action ((stunned stunned))    
    (with-accessors
     ((duration duration)) stunned
     (decf duration)

     (when (<= duration 0)
       (switch-to-state idle))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Block
(defclass+ blocking ()
  ((name :initform "blocking")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High Block

(defclass+ high-block (combined-linear-animation state blocking)
  ((name :initform "high-block")
   (:ia time-pos
	:initform 0)))

(defmethod main-action ((high-block high-block))    
    (with-accessors
     ((time-pos time-pos)) high-block
     (incf time-pos)

     (cond
      ((not (get-held :defense))
       (switch-to-state idle))
      
      ((get-pressed :a1)
      (switch-to-state jab)))))


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
      (kb-direction kb-direction) (decceleration decceleration)) high-block-stun
     (incf time-pos)
     (decf duration)
     (incf (x (parent high-block-stun)) (* kb-speed kb-direction))
     (decf kb-speed decceleration)

     (cond
      ((<= duration 0)
       (switch-to-state high-block)))))