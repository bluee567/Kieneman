(in-package "KIENEMAN")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric begin-frame (actor)
  (:documentation "Asks the actor to preform it's main action, updating its
state for the frame. Note that this state may be altered afterwards by other events acting
upon the actor."))

(defgeneric main-action (actor)
  (:documentation "Asks the actor to preform it's main action, updating its
state for the frame. Note that this state may be altered afterwards by other events acting
upon the actor."))

(defgeneric print-state (state))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fighters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric leading-foot (fighter)
  (:documentation "Returns which foot is leading. Return values can be:
 :left or :right"))

(defclass+ fighter (single-hitbox 2d-vector hp-obj)
  ((:iea name)
   (:ia state
	:initform (make-instance 'idle))
	(:ia buffered-state
		:initform nil)
	(:ia buffer-time
		:initform nil)
   (:ia radius)
   (:ia height)
   
   (:iea left-leg-pos
	 :documentation "The position of the left leg relative to the right leg
in terms of the x axis. Effectivly the value is positive if the left leg would have a higher x value then the right leg,
negitive otherwise. Note that the position of each leg doesn't necessaraly have an actual value, this slot is simply used
to represent the stance the fighter is in so that the 'openness' of the stance relative to the opponent can be determined.")
   (:ia target
	:documentation "The opponent that is being fought. Used to make sure the players is facing the right way.")
   (ogre-entity
    :reader ogre-entity)
   (ogre-node
    :reader ogre-node)
   
   (:iea input-funcs
	:documentation
	"A keylist of functions which, when called, will return input values
which are used to drive the actions of the character.")
	(:ia tensions
	 :initform (make-hash-table))
   (:iea buffered-input-func)
   (:iea input-override-func)
   (:iea death-function
	 :documentation
	 "The function that is called when the object dies.")
   (:ia buffered-input
	:documentation
	"A keylist of functions which return the last input pressed if it occured
while another action was taking place. The input will be executed as soon as the fighter moves
into a neutral or idle position, i.e. if punch was pressed while a kick was preformed the punch
will be executed as soon as the kick finishes."
	:initform
	(let ((nil-fun (λ nil)))
	    (list
	     :a1 nil-fun
	     :a2 nil-fun
	     :defense nil-fun
	     :down nil-fun
	     :up nil-fun
	     :r-left nil-fun
	     :r-right nil-fun)))))

;;The current fighter being addressed by the program
(defvar *fighter* nil)

(defmethod initialize-instance :around ((fighter fighter) &key)
  (let (ogre-mesh scene-node)
    (declare (dynamic-extent ogre-mesh scene-node))
    (CALL-NEXT-METHOD)))

(defmethod initialize-instance :after ((fighter fighter) &key)
  (setf (slot-value fighter 'ogre-entity) (create-entity *mgr* (name fighter) ogre-mesh))
  (setf scene-node (create-child-scene-node (get-root-scene-node *mgr*)))
  (setf (slot-value fighter 'ogre-node) scene-node)
  (attach-object scene-node (ogre-entity fighter))
  (set-cast-shadows (slot-value fighter 'ogre-entity) ())
  (enter-state fighter (state fighter))
  (enter-animation fighter (state fighter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Debug

(defmethod print-state ((fighter fighter))
  (print-state (state fighter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Movement

(defmethod (setf x) (value (fighter fighter))
  (unless (< (abs value) *stage-width*)
    (setf value (* (signum value) *stage-width* )))
  (setf (slot-value fighter 'x) value)
  (nfx (signum value) * *stage-width* - value))

(defmethod (setf y) (value (fighter fighter))
  (when (< value 0.0f0)
    (setf value 0.0f0))
  (setf (slot-value fighter 'y) value))

(defmethod radius ((fighter fighter))
  (slot-value fighter 'radius))

(defun median-x (obj)
  (x obj))

(defun median-y (obj)
  (+ (y obj) (/ (height obj) 2)))

(defun move-obj (obj dx dy)
  "Moves an object's x & y values reletive to its
current position."
  (incf (x obj) dx)
  (incf (y obj) dy))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Direction


(defun get-direction (&optional (fighter *fighter*))
  "Gets the direction the fighter is facing."
  (with-accessors ((x x)) fighter
		  (with-accessors ((tx x)) (target fighter)
		   (if (> x tx)
		       left right))))

;;This pair of methods will redirect any call to 'direction'
;;to the parent of that object, until the parent happens to be a fighter.			 
(defmethod direction (obj)
  (direction (parent obj)))
  
(defmethod direction ((obj fighter))
  (get-direction obj))

(defun direction-to-symbol (dir)
  (if (= dir 1)
      :r-right
    :r-left))

(defun direction-symbol (&optional (fighter *fighter*))
  "Gets the symbol of the direction the fighter is facing.
Values are :left or :right"
  (direction-to-symbol (get-direction fighter)))

(defun opposite (direction)
  "Returns the direction opposite to the argument."
  (- direction))

(defun opposite-symbol (&optional (fighter *fighter*))
  (direction-to-symbol (opposite (get-direction fighter))))

;;Moves the charater forward and returns the linear distance N moved.
(defun move-forward (dist &optional (fighter *fighter*))
  (let* ((dir (get-direction fighter))
	 (prev-pos (x fighter))
	 (move-vel (* dir dist)))
    (incf (x fighter) move-vel)
    (* dir (- move-vel (- (x fighter) prev-pos)))))

(defun move-up (dist &optional (fighter *fighter*))
  (incf (y fighter) dist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Foot Stance Funcs

(defmethod leading-foot ((fighter fighter))
  "Returns which foot is leading. Return values can be:
:left or :right"
  (if (xor
       (= (left-leg-pos fighter) positive)
       (= (get-direction fighter) left))
      left right))

(defun stance-openness (fighter1 fighter2)
  (if (= (left-leg-pos fighter1) (left-leg-pos fighter2))
      open closed))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; State Buffer

(defmethod get-buffered-state (&optional (fighter *fighter*))
  (buffered-state fighter))

(defmethod set-buffered-state (val &optional (fighter *fighter*))
  (setf (buffer-time fighter) (buffer-time-max (state fighter)))
  (setf (buffered-state fighter) val))

(defmethod buffer-time-max (state)
	*buffer-time-max*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Control

(defgeneric set-override-keys (fighter &rest keys))

(defmethod set-override-keys ((fighter fighter) &rest keys)
  (apply (buffered-input-func fighter) keys))

(defgeneric set-input-override (fighter val))

(defmethod set-input-override ((fighter fighter) val)
  (funcall (input-override-func fighter) val))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Collisions

(defun fighter-collision (f1 f2)
  (with-accessors ((bottom1 bottom) (top1 top)(left1 left) (right1 right)) f1
		  (with-accessors ((bottom2 bottom) (top2 top) (left2 left) (right2 right)) f2
				  (and
				   (< bottom1 top2)
				   (< bottom2 top1)
				   (< left1 right2)
				   (< left2 right1)))))

(defmethod collision ((hb1 fighter) (hb2 hitbox))
  (with-accessors ((state state)) hb1
		  (collision state hb2)))

(defmethod collision ((hb2 hitbox) (hb1 fighter))
  (with-accessors ((state state)) hb1
		  (collision state hb2)))

(defmethod collision ((hb1 fighter) (hb2 fighter))
  (fighter-collision hb1 hb2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Death

(defmethod zero-hp ((hp-obj fighter))
  (reset))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Actions


(defdelegate fighter state
  (main-action
   begin-frame
   top bottom
   left right))

(defconstant *fi-knockback* 4.0f0)

(defmethod handle-collision ((f1 fighter) (f2 fighter))
  "Yes, this code is awful"
  (let* ((m (< (x f1) (x f2)))
	 (lf (if m f1 f2)) ;Left fighter
	 (rf (if m f2 f1)) ;Right Fighter
	 (pxl (x lf)) (pxr (x rf)))
    ;This stuff is procedural
    (move-obj lf (- *fi-knockback*) 0.0f0)
    (move-obj rf *fi-knockback* 0.0f0)
       
       (unless (collision f1 f2)
	 (let* ((dist (diff (left rf) (right lf)))
	       (dxl (diff pxl (x lf)))
	       (dxr (diff pxr (x rf)))
	       (rat (mapratio (list dxl dxr)))
	       (wl (car rat))
	       (wr (cadr rat)))
	   ;Imperitive
	      (move-obj lf (* dist wl) 0.0f0)
	      (move-obj rf (- (* dist wr)) 0.0f0)
	      ;; (format t "diff: ~S dxl: ~S dxr: ~S~%xl: ~S xr: ~S~%" dist dxl dxr (x lf) (x rf))
	      ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Animations

(defmethod animate ((obj fighter))
  (with-accessors ((x x) (y y) (radius radius) (width width) (height height) (ogre-node ogre-node) (hp hp) (display display)) obj
		  (set-position-f ogre-node x y 0.0)
		  (reset-orientation ogre-node)
		  (yaw ogre-node
		       (if (equal right (get-direction obj))
			   90.0f0 -90.0f0))
		  (animate (state obj))))

(defmacro set-fighter-mesh (class mesh)
  `(defmethod initialize-instance :before ((obj ,class) &key)
    (setf ogre-mesh ,mesh)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tikaman
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass+ tika (fighter)
  ())

(defmethod initialize-instance :before ((obj tika) &key)
  (setf ogre-mesh "Tikaman.mesh")
  (setf (radius obj) 7.0)
  (setf (height obj) 65.0))

