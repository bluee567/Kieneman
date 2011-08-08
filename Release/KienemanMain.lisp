;;(defpackage "KIENEMAN"(:use "COMMON-LISP" "FFI" "COM.CYBERTIGGYR.PRM"))
(in-package "KIENEMAN")

;(load "Kieneman.lisp")

(DEFAULT-FOREIGN-LANGUAGE :stdc)

(default-foreign-library "OgreTut1.dll")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant right 1)
(defconstant left -1)
(defconstant up 1)
(defconstant down -1)
(defconstant open 1)
(defconstant closed -1)
(defconstant positive 1)
(defconstant negative -1)
(defconstant tau (* 2 pi))
(defconstant *fps* 60.0)
(defconstant *buffer-time-max* 80)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Setup Rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *man-node* nil)
(defvar *mgr* nil)
(defvar *cam* nil)
(defvar *actor-list* nil)

(defvar *p1* nil)
(defvar *p2* nil)

(defvar *frame-delay* 0.0)

(defparameter *stage-width* 300.0f0);The max positive and negative values a player's x value can be.

(defun add-actor (actor)
  (push actor *actor-list*))

(def-call-out setup-background
  (:name "setupBackground")
  (:arguments (cptr scene-manager) (cptr viewport) (width single-float))
  (:return-type nil))

(def-call-out set-hp-bar-value
  (:name "setHPbarValue")
  (:arguments (val int)(bar int))
  (:return-type nil))

(def-call-out set-text-area
 (:name "setTextArea")
 (:arguments (text c-string) (bar int)))

(defun setup-rendering ()
  (let (vp light)
   (setf *mgr* (create-scene-manager 1 "Default SceneManager"))
   
   (setf *cam* (create-camera *mgr* "Camera"))
   (set-cam-position *cam* 0f0 0f0 400f0)
   (look-at *cam* 0f0 0f0 0f0)
   (set-cam-position *cam* 0f0 100f0 400f0)
   (setf vp (add-viewport *cam*))
   (set-background-colour vp 0f0 0f0 0f0 0f0)
   (set-aspect-to-viewport *cam* vp)
   
   (set-ambient-light *mgr* 0f5 0f5 0f5 1f0)   
   (setup-background *mgr* vp *stage-width*)
  
   (setf light (create-light *mgr* "Light1"))
   (set-lt-type light 0)
   (set-lt-position light 0f0 150f0 250f0)
   (set-lt-diffuse-colour light 1f0 1f0 1f0)
   (set-lt-specular-colour light 1f0 1f0 1f0)

   (setf light (create-light *mgr* "Light2"))
   (set-lt-type light 1)
   (set-lt-direction light 0f0 -1f0 -1f0)
   (set-lt-diffuse-colour light 0f75 0f75 0f0)
   (set-lt-specular-colour light 0f75 0f75 0f0)

   (set-text-area "Nothing!" 1)))


(defun remove-dead ()
  "Removes actors from the list if they have 'died'."
  (setf *actor-list*
   (remove-if-not
    'alive
    *actor-list*)))

(defun update-input ()
  (update-input-interface (input-funcs *p1*))
  (update-input-interface (input-funcs *p2*))
  (capture-input))

(defun begin-frame-events ()
  (begin-frame *p1*)
  (begin-frame *p2*))

(defun main-events ()
  (dolist (actor *actor-list*)
    (main-action actor)))

(defun character-collisions ()
	(character-collision (state *p1*) (state *p2*)))

(defun collision-events ()
  (unless (null *actor-list*)
   (labels ((walk-list (list)
		       (unless (null (cdr list))
			 (let ((actor1 (car list))
			       (rlist (cdr list)))
			   (dolist (actor2 rlist)
			     (when (collision actor1 actor2)
			       (handle-collision actor1 actor2)))
			   (walk-list rlist)))))
     (walk-list *actor-list*))))

(defun debug-events ()
  (set-text-area (print-state *p1*) 1)
  (set-text-area (print-state *p2*) 2))

(defconstant *cam-min-range* 250.0f0)
(defconstant *cam-accel-range* 360.0f0)

(defun move-camera ()
  (let* ((x1 (x *p1*)) (x2 (x *p2*))
	 (x (/ (+ x1 x2) 2.0f0))
	 y
	 (z (/ (abs (/ (- x1 x2) 2.0f0)) (tan (/ pi 8.0f0)))))
    (cond ;Restricts the minimum distance of the camera.
     ((< z *cam-min-range*) (setf z *cam-min-range*))
     ((< z *cam-accel-range*) (let ((weight (/ (- z *cam-min-range*) (- *cam-accel-range* *cam-min-range*))))
		     (setf z (+ (* weight z ) (* (- 1.0f0 weight) *cam-min-range*))))))

    (setf y (nfx 35.0f0 + z * 0.1f0))
    
    (set-cam-position *cam* x y z)))

(defun advance-animation ()
  (dolist (actor *actor-list*)
    (animate actor))
  (set-hp-bar-value (hp *p1*) 1)
  (set-hp-bar-value (hp *p2*) 2))

(def-call-out render-frame
  (:name "renderFrame")
  (:return-type boolean))

(define-condition game-yielded ()
  ())

(defun main-loop ()
  (loop
   (progn
     (update-input)
     (begin-frame-events)
     (main-events)
	 ;;Possibly add remove dead here.
	 (character-collisions)
     (collision-events)
     (remove-dead)
     (debug-events)
     (move-camera)
     (advance-animation)
     (if (key-held KC_F4)
	 (return))
     #|(if (key-held KC_EQUALS)
	 (incf *frame-delay* (/ 1 60)))
     (if (key-held KC_MINUS)
	 (progn
	   (decf *frame-delay* (/ 1 60))
	   (if (< *frame-delay* 0)
	       (setf *frame-delay* 0))))|#
     (if (not (render-frame))
	 (restart-case (error 'game-yielded)
		       (continue () nil)
		       (exit () (return)))))))

(defun reset ()
  (setf (hp *p1*) 1000)
  (setf (x *p1*) -50.0)
  (setf (y *p1*) 0.0)

  (setf (hp *p2*) 1000)
  (setf (x *p2*) 50.0)
  (setf (y *p2*) 0.0))

(start-system)
(setup-rendering)
(start-io)