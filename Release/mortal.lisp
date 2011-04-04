(in-package "KIENEMAN")

;;Mortal Objects are capable of being 'killed' by the system and removed from
;;all structures with a remove-dead routine.

(defgeneric alive (entity)
  (:documentation "If an entity is alive then it will continue to persist in any global
data structure that stores entities across frames. An object that is not alive will be deleted
by the start of the next frame."))

(defgeneric kill (entity))

;;Entities are immortal by default.
(defmethod alive (entity)
  t)

;;Entities will not die unless  they are mortal.
(defmethod kill (entity)
 ())

;;Mortal objects can be destroyed by 'killing' them.
(defclass+ mortal ()
  ((:ia alive
	:initform t)))


(defmethod kill (mortal)
  (setf (alive mortal) nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; hp-obj
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass+ hp-obj ()
  ((hp
    :initarg :hp
    :reader hp)
   (:ia max-hp
	:initform 1000)))

(defgeneric (setf hp) (val hp-obj))

(defgeneric zero-hp (hp-obj)
  (:documentation "Called on hp objects when they have
0 hp."))

;;Objects are killed when they have 0 hp by default.
(defmethod zero-hp (hp-obj)
  (kill hp-obj))

(defmethod (setf hp) (val (hp-obj hp-obj))
   (with-accessors
    ((max-hp max-hp)) hp-obj
    (cond
     ((<= val 0) (progn
		   (setf (slot-value hp-obj 'hp) 0)
		   (zero-hp hp-obj)))
     ((< val max-hp) (setf (slot-value hp-obj 'hp) val))
     (t (setf (slot-value hp-obj 'hp) max-hp)))))

(defmethod initialize-instance :after ((hp-obj hp-obj) &key)
  (setf (hp hp-obj) (max-hp hp-obj)))


