(in-package "KIENEMAN")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rec-attack-box
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass+ rec-attack-box (default-hitbox mortal)
  ((:iea parent-state)
   (:iea damage)
   (:iea hitstun)
   (:ia display
	:initform nil)
   (hit-objects
    :initform nil
    :accessor hit-objects)))

(defmethod initialize-instance :after ((box rec-attack-box) &key)
  (with-accessors ((parent-state parent-state) (display display)) box
    (with-accessors((parent parent)) parent-state
     (push parent (hit-objects box))
     (setf display (make-hit-rectangle (x box) (+ (y box) (/ (height box) 2)) (width box) (height box) "red" *mgr*)))))

(defmethod main-action ((rab rec-attack-box))
  t)

(defmethod animate ((obj rec-attack-box))
  t)

(defmethod kill ((box rec-attack-box))
  (when (alive box)
    (destroy-entity *mgr* (display box)))
  (call-next-method))

;;If an attacks box hits a generic object, nothing happens.
(defmethod handle-collision ((rab rec-attack-box) thing)
  t)

(defmethod handle-collision (thing (rab rec-attack-box))
  (handle-collision (rab thing)))

(defmethod handle-collision ((rab rec-attack-box) (fighter fighter))
  (when (= 0 (loop for e in (hit-objects rab) count (eq e fighter)))
    ;;When this hitbox has not previously hit the fighter.
    (push fighter (hit-objects rab)) ;Add fighter to list of hit objects.
    (handle-collision rab (state fighter))))

(defmethod handle-collision ((fighter fighter) (rab rec-attack-box))
  (handle-collision (rab fighter)))


(defmethod handle-collision ((rab rec-attack-box) (state state))
  ;; (hit fighter (parent-state rab))
  (with-accessors ((fighter parent)) state
   (decf (hp fighter) (damage rab))		;Hurt him!
   (change-state fighter (make-instance 'stunned
					:duration (hitstun rab)))))

(defmethod handle-collision ((rab rec-attack-box) (state high-block))
  (with-accessors ((fighter parent)) state		
   (change-state fighter (make-jab-block-stun rab))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass+ jab (single-animation state)
  ((:ia name :initform "jab")
   (:ia hitbox
	:initform nil)
   
   (:ia tpos
	:initform 0)
   (:ia tpos-delay
	:initform 0) ;Used to delay tpos when canceling the attack.
   
   (:ia forward-speed
	:initform 0)
   (:ia key-buffer
	:initform nil)))

(defclass+ jab-box (rec-attack-box)
  ())

(defmethod handle-collision ((rab jab-box) (state idle))
 (with-accessors ((fighter parent)) state		
   (change-state fighter (make-jab-block-stun rab))))

(defun make-jab-block-stun (rab)
 (make-instance 'high-block-stun
		:duration 16
		:kb-direction (get-direction (parent (parent-state rab)))
		:kb-speed 1.0
		:decceleration 0.05))

(defmethod main-action ((obj jab))
  (with-accessors
   ((fighter parent) (tpos tpos) (tpos-delay tpos-delay)) obj
   (with-accessors
    ((x x) (y y)) fighter
    
    
    (if (and (> tpos-delay 0) (>= tpos 6))
	(progn
	  (setf tpos-delay 0)
	  (setf tpos 15)) ;Must eval to an integer to avoid type errors.
      (incf tpos))

    (when (get-pressed :a1)
      (setf (key-buffer obj) :a1))

    (when (or (get-held :down) (get-held :defense) (get-pressed :left) (get-pressed :right))
      (if (< tpos 6)
       (setf tpos-delay 1)
       (setf (key-buffer obj) nil)))
    
    (case tpos
      (8 (let ((hb (make-instance 'jab-box
				  :parent-state obj
				  :damage 50
				  :hitstun 18
				  :x (+ (* (get-direction fighter) 25.0) x) :Y (+ 50.0 y)
				  :radius 5.0 :height 5.0)))
	   (setf (hitbox obj) hb)
	   (add-actor hb)))
      (20 (cond
	   ((eql nil (key-buffer obj)) (switch-to-state idle))
	   ((eql :a1 (key-buffer obj)) (switch-to-state jab))))))))

(defmethod animate ((sa jab))
  (let ((tp (/ (float (tpos sa)) 60.0))
	(ani (animation sa)))
    ;; (format t "Time: ~a " tp)
;;     (format t "Jikan: ~a" (get-time-position ani))
    (set-time-position ani tp)))

(defmethod exit-state ((fighter fighter) (state jab))
  (when (hitbox state)
   (kill (hitbox state))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;roundhouse

(defclass+ roundhouse (single-animation state)
  ((name :initform "roundhouse")
   (:ia hitbox
	:initform nil)
   (:ia tpos
	:initform 0);The time position of the state. 
   (:ia forward-speed
	:initform 0)
   (:ia key-buffer
	:initform nil)))

(defclass+ roundhouse-box (rec-attack-box)
  ())

(defmethod main-action ((obj roundhouse))    
    ;; (cond
;;      ((get-held :h-neutral)
;;       (switch-to-state idle)))

  (with-accessors
   ((fighter parent) (tpos tpos)) obj
   (with-accessors
    ((x x) (y y)) fighter

    (incf tpos)
    (case tpos
      (12 (let ((hb (make-instance 'roundhouse-box
				  :parent-state obj
				  :damage 100
				  :hitstun 18
				  :x (+ (* (get-direction fighter) 30.0) x) :Y (+ 50.0 y)
				  :radius 15.0 :height 10.0)))
	   (setf (hitbox obj) hb)
	   (add-actor hb)))
      (16 (kill (hitbox obj))
	  (setf (hitbox obj) nil))
      (26 (switch-to-state idle))))))

(defmethod exit-state ((fighter fighter) (state roundhouse))
  (when (hitbox state)
   (kill (hitbox state))))

;; (defmethod animate ((roundhouse roundhouse))
;;   (call-next-method)
;;   (add-time (animation roundhouse) (/ 1.0 100.0)))