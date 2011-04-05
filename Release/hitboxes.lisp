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
   
(defun vector2 (ax ay)
   (make-instance '2d-vector
   :x ax :y ay))
   
 (defmethod print-object ((2d-vector 2d-vector) stream)
  (format stream "[x: ~a y: ~a]" (x 2d-vector) (y 2d-vector)))

;;
(defun abs-dist (2d-vec-a 2d-vec-b)
"Euclidian distance metric"
  (let ((x-val (- (x 2d-vec-a) (x 2d-vec-b)))
	(y-val (- (y 2d-vec-a) (y 2d-vec-b))))
   (sqrt (+ (* x-val x-val) (* y-val y-val)))))

;;Ab
(defun ground-dist (2d-vec-a 2d-vec-b)
"Absolute distance between the x values"
  (abs (- (x 2d-vec-b) (x 2d-vec-a))))
  
(defun add (a b)
 "Adds two vectors"
  (vector2 (+ (x a) (x b)) (+ (y a) (y b))))
  
(defun subtract (a b)
 "Subtracts two vectors"
  (vector2 (- (x a) (x b)) (- (y a) (y b))))
  
(defun v-dot (a b)
	(+ (* (x a) (x b))
	 (* (y a) (y b))))
	 
(defun v-length (vec)
	(let ((x (x vec)) (y (y vec)))
	(sqrt (+ (* x x) (* y y)))))
	 
(defun normalise (vec)
	 (let ((len (v-length vec)))
	 (vector2 (/ (x vec) len) (/ (y vec) len))))
	 
(defun v-normal (vec) (vector2 (y vec) (- (x vec))))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Requires: Rectangle (top bottom left right)
;; Impliments: collision

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
;;; triangle-box
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Requires: -
;; Impliments: Triangle

(defclass+ triangular-hitbox (single-hitbox)
	(;A list of 2d-vectors.
	(:ia point-list)))
	
(defclass+ displayed-tribox (triangular-hitbox)
  ((:ia display)))
  
(defun make-displayed-tribox (scalar-list material-name &optional (class 'displayed-tribox))
	(destructuring-bind (x1 y1 x2 y2 x3 y3) scalar-list 
	(make-instance class
		:point-list (list (vector2d x1 y1) (vector2d x2 y2) (vector2d x3 y3))
		:display (make-hit-triangle x1 y1 x2 y2 x3 y3 material-name *mgr*))))
  
(defmethod animate :after ((obj displayed-tribox))
  (with-accessors ((pl point-list) (display display)) obj
		  (destructuring-bind (a b c) pl
		    (update-hit-triangle (x a) (y a) (x b) (y b) (x c) (y c) display))))
			
(defmethod kill ((box displayed-tribox))
  (when (alive box)
    (destroy-manual-object *mgr* (display box)))
  (call-next-method))
	
(labels ((is-overlapping (a b c x)
				(destructuring-bind (d e f) x ;vectors
				(let* ((v (subtract b c));vector
						(n (normalise (v-normal v)));v
						(an (v-dot a n));scaler
						(cn (v-dot c n));s
						(dn (v-dot d n));s
						(en (v-dot e n));s
						(fn (v-dot f n));s
						(anltcn (< an cn));bool
						(vmin (if anltcn an cn)) ;s
						(vmax (if anltcn cn an)));s
					(not (or (and (<= vmax dn) (<= vmax en) (<= vmax fn))
							(and (>= vmin dn) (>= vmin en) (>= vmin fn)))))))
			(side-test (a b)
			"Accepts two point lists."
				(and
					(is-overlapping (first a) (second a) (third a) b)
					(is-overlapping (second a) (third a) (first a) b)
					(is-overlapping (third a) (first a) (second a) b)))
					
			(triangle-collision (pl1 pl2)
				(and (side-test pl1 pl2)
					(side-test pl2 pl1))))
	
	(defmethod collision ((t1 triangular-hitbox) (t2 triangular-hitbox))
		(triangle-collision (point-list t1) (point-list t2)))
		
	(defmethod collision ((t1 triangular-hitbox) (r1 rectangular-hitbox))
		(with-accessors ((top top) (bottom bottom) (left left) (right right)) r1
		(let ((tl (vector2 left top))
				(tr (vector2 right top))
				(bl (vector2 left bottom))
				(br (vector2 right bottom)))
		;;Here the rectangle is treated as two triangles, each one colliding against the triangle that was
		;;not a rectangle to start with.
		(or (triangle-collision (point-list t1) (list tl tr bl))
			(triangle-collision (point-list t1) (list tr bl br))))))
		
	(defmethod collision ((r1 rectangular-hitbox) (t1 triangular-hitbox))
		(triangle-collision t1 r1))
		
	(defun t-collision-example (a1x a1y a2x a2y a3x a3y  b1x b1y b2x b2y b3x b3y)
				(triangle-collision
					(list (vector2 a1x a1y) (vector2 a2x a2y) (vector2 a3x a3y))
					(list (vector2 b1x b1y) (vector2 b2x b2y) (vector2 b3x b3y))))
			
	(defun test-t-collision ()
		(format nil "~a ~a ~a ~a"
		(t-collision-example 0 1   0 0   1 0
							 5 10  10 5  10 10)
		(collision (make-instance 'triangular-hitbox
						:point-list (list (vector2 0 1) (vector2 0 0) (vector2 1 0)))
					(make-instance 'triangular-hitbox
						:point-list (list (vector2 0 1) (vector2 1 0) (vector2 1 1))))
		(collision (make-instance 'triangular-hitbox
						:point-list (list (vector2 0 1) (vector2 0 0) (vector2 1 0)))
					(make-instance 'triangular-hitbox
						:point-list (list (vector2 0 1) (vector2 0 0) (vector2 1 0))))
		(t-collision-example 0 1      0 0      1 0
							 0.5 1.5  0.4 0.5  1.5 0.6))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; displayed-box
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Display must be an object that can be parented to an Ogre node.
(defclass+ displayed-box ()
  ((:ia display)))

(defmethod initialize-instance :after ((box displayed-box) &key)
  (with-accessors ((display display)) box
     (setf display (make-hit-rectangle (x box) (+ (y box) (/ (height box) 2)) (width box) (height box) (material-name box) *mgr*))))

(defmethod animate :after ((obj displayed-box))
  (with-accessors ((x x) (y y) (radius radius) (width width) (height height) (display display)) obj
		  (let ((display-node (get-parent-node display)))
		    (set-position-f display-node x (median-y obj) 0.0)
		    (set-scale display-node width height 1.0))))
			
(defun dbox-cleanup (box)
	(destroy-entity *mgr* (display box))
	(setf (display box) nil))

(defmethod kill :after ((box displayed-box))
  (when (display box)
    (destroy-entity *mgr* (display box))
	(setf (display box) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass+ default-hitbox (rectangular-hitbox 2d-vector low-y-box displayed-box)
  ()) ;This object should be created by any subclasses of default-hitbox. Used to show where hitboxes are.

(defclass+ uni-box (default-hitbox child-hitbox mortal)
  ())

(defmethod animate ((anim uni-box))
  t)
  
(defmethod material-name ((obj uni-box))
	"blue")

#|
(defmethod initialize-instance :after ((box uni-box) &key)
  (with-accessors ((display display)) box
		  (setf display (make-hit-rectangle (x box) (+ (y box) (/ (height box) 2)) (width box) (height box)  *mgr*))))
		  |#