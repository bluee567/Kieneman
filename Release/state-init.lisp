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

(defclass+ state (low-y-box)
  ((:ia parent)
   (:ia radius
	:initform 1.0)
   (:ia height
	:initform 1.0)
   (:ia key-buffer
	:initform (make-instance 'key-bufferer))))

(defclass+ singlebox-state (rectangular-hitbox displayed-box)
  ())

(defclass+ multibox-state (state multi-hitbox)
  ())

(defmethod print-state ((state state))
  (let ((fighter (parent state)))
    (format nil "~a~&x: ~a ~10Ty: ~a~&" state (x fighter) (y fighter))))

(defmethod begin-frame ((state state))
  ())

(defgeneric attack-blocked (state opposing-state)
  (:documentation "Function called when an attack is blocked.
State must be the state of the attacker, while opposing-state
that of the defender. Used to alter the state of an attack
when blocked."))

(defgeneric linear-tracking (state)
  (:documentation "Returns true if the current state
allows active tracking of the opponent's position.
If false, then sidesteps become effective."))

(defmethod linear-tracking ((state state))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;A mixin class which allows an attack state to not linearly track the
;;opponent's position, potentially allowing for the attack to miss even if
;;the appropriate hitboxes overlap.

(defgeneric side-spd (state))

(defmethod side-spd ((state state))
  0.0)

(defclass+ partial-tracking ()
  ((:ia side-dist :initform 0.0)))

(defmethod main-action :before ((pt partial-tracking))
  (incf (side-dist pt) (side-spd (state (target (parent pt)))))
  (format t "~& side-dist: ~a~&" (side-dist pt)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;A mixin class which adds a two component velocity (speed and direction) to a class

(defclass+ component-velocity ()
  ((:ia dir)
   (:ia spd)))

(defmethod  initialize-instance :after ((state component-velocity) &key vel)
  (setf (spd state) (abs vel))
  (setf (dir state) (signum vel)))

  ;;Returns the velocity of the state reletive to world coordinates.
(defmethod real-vel ((state state))
  0.0)

(defmethod real-vel ((state component-velocity))
  (* (vel state) (get-direction (parent state))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;A mixin class which adds a simple linear timeline to a class

(defclass+ linear-timer ()
  ((:ia tpos :initform 0)))

(defmethod main-action :before ((state linear-timer))
  (incf (tpos state)))

(defmacro between (start end &body body)
  (if body
      `(when (and (> tpos ,start) (< tpos ,end))
	 ,@body)
    `(and (> tpos ,start) (< tpos ,end)))) ;Causes the function to eval to true in the absence of a body.

(defmacro within (start end &body body)
  (if body
      `(when (and (>= tpos ,start) (<= tpos ,end))
	 ,@body)
    `(and (>= tpos ,start) (<= tpos ,end)))) ;Causes the function to eval to true in the absence of a body.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;A mixin class which adds a single attack hitbox to a class.

(defclass+ single-attack-box ()
  ((:ia hitbox :initform nil)))

(defmacro set-hitbox (box)
  `(let ((hb ,box))
     (setf hitbox hb)
     (add-actor hb)))

(defmacro remove-hitbox ()
  `(progn
     (when hitbox
	 (kill hitbox))
     (setf hitbox nil)))

(defmethod exit-state :after ((fighter fighter) (state single-attack-box))
  (when (hitbox state)
    (kill (hitbox state))))

(defmethod attack-blocked ((state single-attack-box) opposing-state)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;A mixin class which adds movement-independent-animation to a class.
;;This will move the root node backward the distance the character moves
;;forward.

(defclass+ movement-independent-animation ()
  ((:ia animation-distance :initform 0.0)))

(defmethod animate ((anim movement-independent-animation))
  (ccnm)
  (set-position-f (ogre-node (parent anim)) (- (x anim) (animation-distance anim)) (y anim) 0.0))

(defmacro animate-forward (dist)
  `(let ((dist-vec (* ,dist (get-direction fighter))))
     (incf x dist-vec)
     (incf animation-distance dist-vec)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;A mixin class which adds key-buffering to a class.

(defclass+ key-bufferer ()
  ((:ia key-label :initform nil)
   (:ia pressed :initform t) ;;Whether the key was pressed (t) or released (nil).
   (:ia others-held :initform ())))

(defun get-tappedf (key-buffer key &key released others)
  (with-accessors ((key-label key-label) (pressed pressed) (others-held others-held)) key-buffer
		  ;;NOTE: Others are not currently considered.
		  (if (not released)
		      (or (get-pressed key)
			  (and (equalp key key-label) pressed))
		    (or (get-released key)
			(and (equalp key key-label) (not pressed))))))

(defmacro get-tapped (key &key released others)
  `(get-tappedf key-buffer ,key :released ,released :others ,others))

(defmacro get-untapped (key &key others)
  `(get-tappedf key-buffer ,key :released t :others ,others))

(defun reset-key-buffer (key-buffer)
  (with-accessors ((key-label key-label) (pressed pressed) (others-held others-held)) key-buffer
		  (setf key-label nil)
		  (setf pressed t)
		  (setf others-held ())))

(defun set-tappedf (key-buffer key &key released others)
  (with-accessors ((key-label key-label) (pressed pressed) (others-held others-held)) key-buffer
		  (setf key-label key)
		  (setf pressed (not released))
		  (setf others-held others)))

(defmacro set-tapped (key &key released others)
  `(set-tappedf key-buffer ,key :released ,released :others ,others))

(defmacro set-untapped (key &key others)
  `(set-tappedf key-buffer ,key :released t :others ,others))

(defmacro tapped-list (&rest key-list)
  `(progn
     ,@(mapcar #'(lambda (key)
		  `(if (get-pressed ,key)
		       (set-tapped ,key)))
	      key-list)))

(defmacro untapped-list (&rest key-list)
  `(progn
     ,@(mapcar #'(lambda (key)
		  `(if (not (get-pressed ,key))
		       (set-untapped ,key)))
	      key-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;For states which can be stunned by a hit (inducing a kind of recovery).
;;The stunn however will not switch states, instead it will simply alter the
;;stun-recover variable.

(defclass+ stunnable ()
  ((:ia initial-stun-recovery :initform 0)
   (:ia stun-recovery :initform 0)))

(defmethod set-initial-stun-recovery ((state stunnable) value)
  (setf (slot-value state 'initial-stun-recovery) value)
  (setf (slot-value state 'stun-recovery) value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;For states, enter state should be used in place of initialise-instance.

(defmethod enter-state :after ((f1 fighter) (state single-hitbox))
  (with-accessors ((display display)) state
    (setf display (make-hit-rectangle (x state) (median-y state) (width state) (height state) "blue" *mgr*))))

(defmethod exit-state :after ((f1 fighter) (state single-hitbox))
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


(macrolet
    ((around-action ()
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

(defmacro switch-to-state (state-name &rest args)
  "Switches states. State neme must either be a list (which will be evaluated) or a
single symbol which will be automatically quoted."
  (let ((sname (if (listp state-name) state-name `(quote ,state-name))))

   `(progn
      (let ((new-state (make-instance ,sname ,@args)))
	(change-state *fighter* new-state)))))


;;Copied from Practical Common Lisp
(defun direct-slots (name)
  (copy-list (get name 'slots)))

(defun inherited-slots (name)
  (loop for super in (get name 'supers)
        nconc (direct-slots super)
        nconc (inherited-slots super)))

(defun all-slots (name)
  (nconc (direct-slots name) (inherited-slots name)))

(defun new-class-all-slots (slots superclasses)
  (nconc (mapcan #'all-slots superclasses) (mapcar #'first slots)))

#|
;;NOTES:

alt-name allows the default animation to be overridden and replaced with another.

|#

(defmacro defstate (name &key slots rest supers constants funcs main-action initfunc entryfunc (type 'state) (hb 'singlebox-state) (animation 'combined-linear-animation) (linear t) alt-name)
  (let* ((symbol-name (intern (string-upcase name)))
	 (supers (append (list type hb) supers (list animation) (when linear '(linear-timer))))
	 (slot-names (new-class-all-slots slots supers))
	 (accessors (cons '(fighter parent) (mapcar #'(lambda (a) (list a a)) slot-names)))
	 (print-str (apply 'concatenate (cons 'string (mapcar #'(lambda (slot) (concatenate 'string (string slot) ": ~a ")) slot-names))))
	 (print-vals (mapcar #'(lambda (slot) `(,slot state)) slot-names)))
    
   `(let* ,constants
      
      (defclass+ ,symbol-name ,supers
	,(cons
	  `(name :initform (if ,alt-name
			       ,alt-name
			     ,name))
	  (mapcar #'(lambda (a) (cons :ia a))
		  slots)))

      (macrolet ((func-body (body)
			    `(with-accessors
			      ,',accessors state
			      (with-accessors
			       ((x x) (y y)) fighter
			       ,@body)))
		 
		 (def-statemeth (title params &body body)
		   `(defmethod ,title ((state ,',symbol-name) ,@params)
		      (func-body ,body)))
		 
		 (make-symbol-flet (flist &rest rest)
				   (if flist
				       (let ((name (caar flist))
					     ;;(args (cadar flist))
					     (body (cdar flist)))
					 `(flet ((,name (state) (func-body ,body)))
					    (symbol-macrolet ((,name (,name state)))
					      (make-symbol-flet ,(cdr flist) ,@rest))))
				     `(progn ,@rest))))

	(make-symbol-flet
	 ,funcs
	 
	 (def-statemeth main-action ()
	   ,@main-action)

	 (def-statemeth print-state ()
	   (format nil ,(concatenate 'string "~a~&" print-str "~&") (ccnm) ,@print-vals))

	 ,(when initfunc
	    (let ((keyargs (car initfunc))
		  (bodyforms (cadr initfunc)))
	      `(defmethod initialize-instance :after ((state ,symbol-name) ,@keyargs)
		 ,@bodyforms)))

	 ,(when entryfunc
	    `(defmethod enter-state :after ((fighter fighter) (state ,symbol-name))
	       ,entryfunc))
	
	 ,rest)))))

(defun make-uni-box (p x y r h)
  (make-instance 'uni-box
		 :parent p
		 :x x :y y
		 :radius r :height h))

(defmacro make-hitbox-list (&rest hitbox-forms)
  `(list
    ,@(mapcar
       #'(lambda (form)
	   (destructuring-bind (x y r h &optional binding) form
	     `(make-instance 'uni-box
			     :parent state
			     :x ,x :y ,y
			     :radius ,r :height ,h)))
       hitbox-forms)))

;; (defmacro make-hitbox-list (&rest hitbox-forms)
;;   `(list
;;     ,@(mapcar
;;        #'(lambda (form)
;; 	   (if (listp form)
;; 	       `(apply #'make-uni-box ,(append '(state) form))
;; 	    ;; (destructuring-bind (x y r h &optional binding) form
;; ;; 	      `(make-instance 'uni-box
;; ;; 			      :parent state
;; ;; 			      :x ,x :y ,y
;; ;; 			      :radius ,r :height ,h))
;; 	    form))
;;        hitbox-forms)))

(defmacro set-hitbox-list (&rest hitbox-forms)
  "When applied to a state with a multihitbox,
this macro will set the hitbox-list to one determined
by the hitbos forms passed to it."
  `(setf (hitbox-list state)
	 (make-hitbox-list ,@hitbox-forms)))