(in-package "KIENEMAN")

(defgeneric collision (hitbox1 hitbox2)
  (:documentation "Returns true if the two hitboxes have collided
false otherwise."))

(defclass hitbox () ())

(defgeneric top (hitbox)
  (:documentation "Returns the top extremity of a hitbox"))

(defgeneric bottom (hitbox)
  (:documentation "Returns the bottom extremity of a hitbox"))

(defgeneric left (hitbox)
  (:documentation "Returns the left extremity of a hitbox"))

(defgeneric right (hitbox)
  (:documentation "Returns the right extremity of a hitbox"))

(defgeneric width (obj)
  (:documentation "The width of a nobject (diffrence between its
left and right)."))

(defmethod width (obj)
  (* 2 (radius obj)))

(defgeneric set-dimentions (hitbox))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass+ 2d-vector ()
  ((:iea x)
   (:iea y)))

(defun abs-dist (2d-vec-a 2d-vec-b)
  (let ((x-val (- (x 2d-vec-a) (x 2d-vec-b)))
	(y-val (- (y 2d-vec-a) (y 2d-vec-b))))
   (sqrt (+ (* x-val x-val) (* y-val y-val)))))

(defun ground-dist (2d-vec-a 2d-vec-b)
  (abs (- (x 2d-vec-b) (x 2d-vec-a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass+ single-hitbox (hitbox)
  ())

(defclass+ child-hitbox (2d-vector)
  ((:ia parent)))

(defmethod x ((box child-hitbox))
  (+ (slot-value box 'x) (x (parent box))))

(defmethod y ((box child-hitbox))
  (+ (slot-value box 'y) (y (parent box))))

(defmethod radius ((box child-hitbox))
  (* (slot-value box 'radius) (radius (parent box))))

(defmethod height ((box child-hitbox))
  (* (slot-value box 'height) (height (parent box))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass+ multi-hitbox (hitbox)
  ((:ia hitbox-list
	:initform nil
	 :documentation "A list of hitboxes which are to be tested against
in order to form the multibox.")))

(defmethod collision ((hb1 multi-hitbox) (hb2 single-hitbox))
  (loop for box in (hitbox-list hb1)
	when (collision box hb2)
	return t))

(defmethod collision ((hb1 single-hitbox) (hb2 multi-hitbox))
  (collision hb2 hb1))

(defmethod collision ((hb1 multi-hitbox) (hb2 multi-hitbox))
  (loop for box in (hitbox-list hb2)
	when (collision hb1 box)
	return t))

(defmethod animate :after ((obj multi-hitbox))
  (loop for box in (hitbox-list obj)
	do (animate box)))

;; (defmethod enter-state :after (fighter (obj multi-hitbox))
;;   (set-visible ( (parent obj))))

(defmethod exit-state :after (fighter (obj multi-hitbox))
  (loop for box in (hitbox-list obj)
	do (kill box)))

(defmacro default-hitboxes ()
  `(set-hitbox-list (0.0 0.0 1.0 0.4)
		   (0.0 (* 0.4 (height fighter)) 0.65 0.4)
		   (0.0 (* 0.8 (height fighter)) 0.5 0.2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass+ rectangular-hitbox (single-hitbox)
  ())

(defmethod collision ((hb1 rectangular-hitbox) (hb2 rectangular-hitbox))
  (with-accessors ((bottom1 bottom) (top1 top)(left1 left) (right1 right)) hb1
		  (with-accessors ((bottom2 bottom) (top2 top) (left2 left) (right2 right)) hb2
				  (and
				   (< bottom1 top2)
				   (< bottom2 top1)
				   (< left1 right2)
				   (< left2 right1)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;low-y-box methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Requires: 2d-vector (accessors - x y)
;; Impliments: Rectangle (top bottom left right)

(defclass+ low-y-box ()
  ((:ia radius)
   (:ia height)))

(defmethod bottom ((low-y-box low-y-box))
  (y low-y-box))

(defmethod top ((low-y-box low-y-box))
  (+ (y low-y-box) (height low-y-box)))

(defmethod left ((low-y-box low-y-box))
  (- (x low-y-box) (radius low-y-box)))

(defmethod right ((low-y-box low-y-box))
  (+ (x low-y-box) (radius low-y-box)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; displayed-box
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass+ displayed-box ()
  ((:ia display)))

(defmethod animate :after ((obj displayed-box))
  (with-accessors ((x x) (y y) (radius radius) (width width) (height height) (display display)) obj
		  (let ((display-node (get-parent-node display)))
		    (set-position-f display-node x (median-y obj) 0.0)
		    (set-scale display-node width height 1.0))))

(defmethod kill ((box displayed-box))
  (when (alive box)
    (destroy-entity *mgr* (display box)))
  (call-next-method))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass+ default-hitbox (rectangular-hitbox 2d-vector low-y-box displayed-box)
  ()) ;This object should be created by any subclasses of default-hitbox. Used to show where hitboxes are.

(defclass+ uni-box (default-hitbox child-hitbox mortal)
  ())

(defmethod animate ((anim uni-box))
  t)

(defmethod initialize-instance :after ((box uni-box) &key)
  (with-accessors ((display display)) box
		  (setf display (make-hit-rectangle (x box) (+ (y box) (/ (height box) 2)) (width box) (height box) "blue" *mgr*))))