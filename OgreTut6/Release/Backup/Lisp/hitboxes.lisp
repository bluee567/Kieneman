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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass+ single-hitbox (hitbox)
  ())

(defclass+ child-hitbox (2d-vector)
  ((:iea parent)))

(defmethod x ((box child-hitbox))
  (+ (slot-value box 'x) (x (parent box))))

(defmethod y ((box child-hitbox))
  (+ (slot-values box 'y) (y (parent box))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass+ multi-hitbox (hitbox 2d-vector)
  ((:ia hitbox-list
	:initform nil
	 :documentation "A list of hitboxes which are to be tested against
in order to form the multibox.")
   (:ia top
	:initform 0.0)
   (:ia bottom
	:initform 0.0)
   (:ia left
	:initform 0.0)
   (:ia right
	:initform 0.0)))

(defmethod initialize-instance :after ((box multi-hitbox) &key)
  (set-dimentions box))

(defmethod bottom ((box multi-hitbox))
  (+ (y box) (bottom box)))

(defmethod top ((box multi-hitbox))
  (+ (y box) (top box)))

(defmethod left ((box multi-hitbox))
  (+ (x box) (left box)))

(defmethod right ((box multi-hitbox))
  (+ (x box) (right box)))

(defmethod width ((box multi-hitbox))
  (- (right box) (left box)))

(defmethod set-dimentions ((hbox multi-hitbox))
  (loop for box in (hitbox-list hbox)
	do (progn
	     (when (> (top box) (top hbox))
	       (setf (top hbox) (- (top box) (y hbox))))
	     (when (< (bottom box) (bottom hbox))
	       (setf (bottom hbox) (- (bottom box) (y hbox))))
	     (when (> (right box) (right hbox))
	       (setf (right hbox) (- (right box) (x hbox))))
	     (when (< (left box) (left hbox))
	       (setf (left hbox) (- (left box) (x hbox)))))))

(defmethod collision ((hb1 multi-hitbox) (hb2 single-hitbox))
  (with-accessors
   ((bottom1 bottom) (top1 top)(left1 left) (right1 right) (list1 hitbox-list)) hb1
   (with-accessors
    ((bottom2 bottom) (top2 top) (left2 left) (right2 right) (list2 hitbox-list)) hb2
    (if (and
	 (< bottom1 top2)
	 (< bottom2 top1)
	 (< left1 right2)
	 (< left2 right1))
	(loop for box in hb1
	      when (collision box hb2)
	      return t)
      nil))))

(defmethod collision ((hb1 single-hitbox) (hb2 multi-hitbox))
  (collision hb2 hb1))

(defmethod collision ((hb1 multi-hitbox) (hb2 multi-hitbox))
  (with-accessors
   ((bottom1 bottom) (top1 top)(left1 left) (right1 right) (list1 hitbox-list)) hb1
   (with-accessors
    ((bottom2 bottom) (top2 top) (left2 left) (right2 right) (list2 hitbox-list)) hb2
    (if (and
	 (< bottom1 top2)
	 (< bottom2 top1)
	 (< left1 right2)
	 (< left2 right1))
	(loop for box in hb2
	      when (collision hb1 box)
	      return t)
      nil))))


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
		    (set-position-f display-node x (median-y obj) -25.0)
		    (set-scale display-node width height 1.0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass+ default-hitbox (rectangular-hitbox 2d-vector low-y-box displayed-box)
  ()) ;This object should be created by any subclasses of default-hitbox. Used to show where hitboxes are.

