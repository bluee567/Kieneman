(in-package "KIENEMAN")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Animation

(defclass animation () ())

(defmethod transition-animation ((f1 fighter) (old-anim animation) (new-anim animation))
  (exit-animation f1 old-anim)
  (enter-animation f1 new-anim))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Single Animation

(defclass+ single-animation (animation)
  ((:iea name)
   (:ia animation)
   (:ia animation-incr :initform 1.0)))

(defdelegate single-animation animation
  (get-weight
   (set-weight weight)
   (add-time dtime)
   get-time-position
   (set-time-position time)
   get-enabled
   (set-enabled bool)))

(defmethod animate ((sa single-animation))
  (add-time (animation sa) (* (animation-incr sa) (/ 1.0 60.0))))

(defmethod enter-animation ((f1 fighter) (new-state single-animation))
  (setf (animation new-state) (set-animation (ogre-entity f1) (name new-state) t))
  (set-time-position (animation new-state) 0.0))

(defmethod exit-animation ((f1 fighter) (old-anim single-animation))
  (set-enabled (animation old-anim) nil))




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
	:initform 0.25)))

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

(defmethod transition-animation ((f1 fighter) (old-anim animation) (new-anim combined-animation))
  (setf (prev-animation new-anim) old-anim)
  (enter-animation f1 new-anim))

(defmethod enter-animation ((f1 fighter) (new-state combined-animation))
  (setf (animation new-state) (set-animation (ogre-entity f1) (name new-state) t))
  (set-time-position (animation new-state) 0.0))

(defmethod exit-animation ((f1 fighter) (old-anim combined-animation))
  (set-enabled (animation old-anim) nil)
  (when (not (eql (prev-animation old-anim) nil))
   (exit-animation f1 (prev-animation old-anim))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multi Animation
;;
;; An animation which itself contains oter animations, allowing a single
;; state to use various animations without swapping to another (state).

(defclass+ multi-animation (animation)
  ((:ia anim-list
	:initform nil) ;;Keylist of animations
   (:ia current-animation)))

(defdelegate multi-animation current-animation
  (get-weight
   (set-weight weight)
   (add-time dtime)
   get-time-position
   (set-time-position time)
   get-enabled
   (set-enabled bool)))

(defmethod animate ((sa multi-animation))
  (if (not (listp (current-animation sa)))
      (animate (current-animation sa))
    (dolist (anim (current-animation sa))
      (animate anim))))

(defmethod main-action :around ((state multi-animation))
  (labels ((switch-animation (key)
			     (let ((next-anim (getf (anim-list state) key)))
			      (transition-animation (parent state) (current-animation state) next-anim)
			      (setf (current-animation state) next-anim))))
    (call-next-method)))


(defmethod enter-animation ((f1 fighter) (new-state multi-animation))
  (setf (current-animation new-state) (cadr (anim-list new-state)))
  (enter-animation f1 (current-animation new-state))
  (set-time-position (current-animation new-state) 0.0))


(defmethod exit-animation ((f1 fighter) (old-anim multi-animation))
  (exit-animation f1 (current-animation old-anim)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; "A simple linear animation that blends in the 
;; previous animation."


(defclass+ combined-linear-animation (combined-animation)
  ((:ia animation-incr
	:initform 1.0)))

(defmethod animate  ((ca combined-linear-animation))
  (call-next-method)
  (add-time (animation ca) (* (animation-incr ca) (/ 1.0 60.0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper Macros

(defmacro scale-animation (to-vals from-vals)
 `(progn
    (ccnm)
    (let* ((mtp (/ (map-through-range (float tpos) ,to-vals ,from-vals) *fps*)))
      (set-time-position animation mtp))))


