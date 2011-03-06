(in-package "KIENEMAN")


;;NOTE: It is important to consider what happens when the weight of a sub animation is changed
;;when that animation is contained in multiple states.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Animation

(defclass animation () ())

(defmethod transition-animation ((f1 fighter) (old-anim animation) (new-anim animation))
  
  ())




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
     (set-weight anim weight)
     ;; (add-time panim (/ 1.0 60.0))
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



(defmacro get-anim (key)
  `(get-animation state ,key))

(defmethod get-animation ((state multi-animation) key)
  (getf (anim-list state) key))

;; (defmacro get-anim (key)
;;   "Obtains one of the animations within a multi animation via its
;; key."
;;   `(getf (anim-list state) ,key))

(defmacro get-current-animation ()
  `(current-animation state))

(defmacro set-current-animation (key)
  `(progn
     (exit-animation (parent state) (current-animation state))
     (setf (current-animation state) (get-anim ,key))
     (enter-animation (parent state) current-animation)))


(defmethod enter-animation ((f1 fighter) (new-state multi-animation))
  (setf (current-animation new-state) (cadr (anim-list new-state)))
  (enter-animation f1 (current-animation new-state))
  (set-time-position (current-animation new-state) 0.0))


(defmethod exit-animation ((f1 fighter) (old-anim multi-animation))
  (exit-animation f1 (current-animation old-anim)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Combined Multi Animation
;;
;; An animation which itself contains oter animations, allowing a single
;; state to use various animations without swapping to another (state).
;; Each animation will combine with other animations in such a way that
;; you are able to control the animation's weight

(defclass+ combined-multi-animation (animation)
  ((:ia anim-list
	;;An assoc list containing aimations and their names.
	:initform nil)
   (:ia state-weight
	:initform 1.0)
   (:ia enabled
	:initform t)))

;; (defdelegate multi-animation current-animation
;;   (get-weight
;;    (set-weight weight)
;;    (add-time dtime)
;;    get-time-position
;;    (set-time-position time)
;;    get-enabled
;;    (set-enabled bool)))

(defmethod enter-state :after ((fighter fighter) (anim combined-multi-animation))
  (let ((entity (ogre-entity (parent anim))))
    ;;Takes a list of names (from anim names) and creates an assoc list where each pair
    ;;consists of a name and the animation state 
    (setf (anim-list anim)
	  (mapcar #'(lambda (name) (cons name (get-animation-state entity name)))
		  (anim-list anim)))))

;; (defmethod initialize-instance :after ((anim combined-multi-animation) &key)
;;   (let ((entity (ogre-entity (parent anim))))
;;     ;;Takes a list of names (from anim names) and creates an assoc list where each pair
;;     ;;consists of a name and the animation state 
;;     (setf (anim-list anim)
;; 	  (mapcar #'(lambda (name) (cons name (get-animation-state entity name)))
;; 		  (anim-list anim)))))

(defmethod get-weight ((anim combined-multi-animation))
  (state-weight anim))

(defmethod set-weight ((anim combined-multi-animation) weight)
  ;; (if (equal 0.0 (state-weight anim))
;;       (set-enabled anim nil)
;;       (let ((ratio (/ weight (state-weight anim))))
;; 	(dolist (anim-cons (anim-list anim))
;; 	  (let ((anim-state (cdr anim-cons)))
;; 	    (set-weight anim-state (* ratio (get-weight anim-state)))))
;; 	(setf (state-weight anim) weight)))
  (setf (state-weight anim) weight))

(defmethod get-enabled ((anim combined-multi-animation))
  (enabled anim))

(defmethod set-enabled ((anim combined-multi-animation) bool)
  (setf (enabled anim) bool)
  (dolist (anim-cons (anim-list anim))
    (set-enabled (cdr anim-cons) bool)))

;; (defmethod get-anim ((sa combined-multi-animation) name)
;;   (assoc name (anim-list sa) :test #'string=))

(defmethod animate ((sa combined-multi-animation))
  ;; (dolist (anim-cons (anim-list sa))
;;     (animate anim))
  ())

(defmethod main-action :around ((state combined-multi-animation))
  (labels ((set-animation-weight (name weight)
				 (let ((next-anim (assoc name (anim-list state) :test #'string=)))
				   (set-weight next-anim weight)
				   (set-enabled next-anim (not (equal 0.0 weight))))))
    (call-next-method)))

(defmethod get-animation ((sa combined-multi-animation) name)
		     (assoc name (anim-list sa) :test #'string=))

(defmethod enter-animation ((f1 fighter) (new-state combined-multi-animation))
  (set-enabled new-state t)
  (dolist (anim-cons (anim-list new-state))
     (set-time-position (cdr anim-cons) 0.0)))

;; (defmethod enter-animation :after ((f1 fighter) (new-state combined-multi-animation))
  
;;   ;; (set-time-position (current-animation new-state) 0.0)
;;   )


(defmethod exit-animation ((f1 fighter) (old-anim combined-multi-animation))
  (dolist (anim-cons (anim-list old-anim))
     (set-enabled (cdr anim-cons) nil)))


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

(defmacro scale-animation (to-vals from-vals)
 `(progn
    (ccnm)
    (let* ((mtp (/ (map-through-range (float tpos) ,to-vals ,from-vals) *fps*)))
      (set-time-position animation mtp))))




