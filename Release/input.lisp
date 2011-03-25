(in-package "KIENEMAN")

(DEFAULT-FOREIGN-LANGUAGE :stdc)
(default-foreign-library "OgreTut1.dll")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant centered #x0)
(defconstant north #x1)
(defconstant south #x10)
(defconstant east #x100)
(defconstant west #x1000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initialisation/Cleanup functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-call-out test1
  (:name "test1")
  (:arguments)
  (:return-type int))

(def-call-out start-system
  (:name "startSystem")
  (:arguments)
  (:return-type nil))

(def-call-out start-io
  (:name "startIO")
  (:arguments)
  (:return-type nil))

(def-call-out end-application
  (:name "endApplication")
  (:arguments)
  (:return-type nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Input interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-call-out capture-input
  (:name "capture_input")
  (:return-type nil))

(def-call-out toggle-held
  (:name "toggleHeld")
  (:arguments (itog input-toggle))
  (:return-type boolean))

(def-call-out key-held
  (:name "keyHeld")
  (:arguments (kc keycode))
  (:return-type boolean))

(def-call-out button-held
  (:name "buttonHeld")
  (:arguments (button int) (joystick int))
  (:return-type boolean))

(def-call-out direction-held
  (:name "directionHeld")
  (:arguments (hat int) (joystick int))
  (:return-type boolean))

(def-call-out return-dir
  (:name "returnDir")
  (:arguments (joystick int))
  (:return-type int))

(def-call-out slider-val
  (:name "sliderVal")
  (:arguments (slider int) (joystick int))
  (:return-type int))

;;Returns a value in the approximate range [-0.5, 0.5]
(def-call-out axis-val
  (:name "axisVal")
  (:arguments (axis int) (joystick int))
  (:return-type single-float))

(def-call-out no-of-hats
  (:name "noOfHats")
  (:arguments (joystick int))
  (:return-type short))

(def-call-out no-of-axes
  (:name "noOfAxes")
  (:arguments (joystick int))
  (:return-type short))

(def-call-out no-of-buttons
  (:name "noOfButtons")
  (:arguments (joystick int))
  (:return-type short))


(defun create-input-key (func label)
  (list :func func :current-state nil :prev-state nil :label label))

(defun get-kb-input-func (kc)
  "Returns a function which returns true if the key
corresponding to kc is held down."
  (λ (key-held kc)))

(defun get-joy-input-func (button joystick)
  "Returns a function which returns true if the pad/stick key
corresponding to kc is held down."
  (λ (button-held button joystick)))

(defun get-direction-input-func (direction joystick)
  "Returns a function which returns true if the pad/stick key
corresponding to kc is held down."
  (λ (= (boole boole-and (return-dir joystick) direction) direction)))

(defparameter *slider-max-val* 100)

(defun get-slider-input-func (slider joystick)
  (λ (slider-val slider joystick)))

;;NOTE: Must be a float to preform the correct division operation.
(defparameter *axis-max-val* 0.5)

(defun get-axis-input-func (axis joystick)
  (λ (axis-val axis joystick)))

(defun filtered-axis-func (axis joystick &key (dir 1.0) (max 1.0) (dead-range 0.1) (sticky-range 0.001))
  (λ (let ((pos (* dir (/ (axis-val axis joystick) *axis-max-val*))))
       (if (>= pos dead-range)
	   (if (> pos (- max sticky-range))
	       max
	     (/ (- pos dead-range) (- max dead-range)))
	   nil))))

(defun update-input-interface (interface)
  (loop for key in interface
	when (listp key) do 
	(progn (setf (getf key :prev-state) (getf key :current-state))
	       (setf (getf key :current-state) (funcall (getf key :func))))))

(abbrev get-kbi get-kb-input-func)
(abbrev get-ji get-joy-input-func)
(abbrev get-di get-direction-input-func)
(abbrev get-si get-slider-input-func)
(abbrev get-ai filtered-axis-func)

(defvar *left* (get-kbi KC_LEFT))
(defvar *right* (get-kbi KC_RIGHT))
(defvar *up* (get-kbi KC_UP))
(defvar *down* (get-kbi KC_DOWN))
(defvar *front* (get-kbi KC_D))
(defvar *back* (get-kbi KC_S))

(defparameter *keymap* nil)

;;NOTES:
;;
;; Prefix h- stands for horizontal, i.e. left and right direction or the x plane.
;;
;; Prefix r- stands for 'raw', meaning that it is directly obtaining the input
;; from the input function. However when getting a directional input, the keyboard
;; makes it logicaly possible to press two opposite directions at the same time.
;; To take this into account, non raw input functions (those without an r- prefix)
;; will obtain the logical direction instead of the raw directional input. This means
;; that passing :left to get-held will return nil if both :r-right and :r-left return
;; true.
;;
;; nothing means that no raw direction is being pressed.
;;
;; double means that both directions of an axis are being pressed.
;;
;; neutral means that either no direction or both directions are being pressed. i.e.
;; neither logical left or logical right (or up/down) are being pressed, though both
;; buttons may be physically held down.

(defun get-key-state (key state-key &optional (keymap *keymap*))
  "
NOTE: keymap is a dynamic variable and hence a keymap
variable must be lexicaly bound at the point of invocation."
  (flet ((call-func (sym)
		    (getf (getf keymap sym) state-key)))
   (cond
    ((eql key :left)
     (and (not (call-func :r-right))
	  (call-func :r-left)))

    ((eql key :right)
     (and (not (call-func :r-left))
	  (call-func :r-right)))
   
    ((eql key :h-double)
     (and (call-func :r-right)
	  (call-func :r-left)))
    ((eql key :h-nothing)
     (not (or (call-func :r-right)
	      (call-func :r-left))))

    ((eql key :h-neutral)
     (mor get-held (:h-double) (:h-nothing))) ;Yes, it's completly intentional ;)
   
    (t (getf (getf *keymap* key) state-key)))))

(defun get-keymap ()
  *keymap*)

(defmacro with-keymap (km &body body)
  `(let ((*keymap* ,km))
     ,@body))

(defun get-held (key &optional (keymap *keymap*) (not-past t))
  "Calls the corresponding 'held' func from the current keymap."
  (if not-past
      (get-key-state key :current-state keymap)
    (get-key-state key :prev-state keymap)))

(defun get-pressed (key &optional (keymap *keymap*))
  (and
   (get-key-state key :current-state keymap)
   (not (get-key-state key :prev-state keymap))))

(defun get-released (key &optional (keymap *keymap*))
  (and
   (get-key-state key :prev-state keymap)
   (not (get-key-state key :current-state keymap))))

(defun get-label (key)
  "Gets the lable of the key in the current keymap.
NOTE: keymap is a dynamic variable and hence a keymap
variable must be lexicaly bound at the point of invocation."
  (getf (getf *keymap* key) :label))

(defun convert-to-numeric (raw-val)
  (cond
   ((numberp raw-val) raw-val)
   ((not raw-val) 0.0)
   (t 1.0)))

(defun get-numeric (key &optional (keymap *keymap*) (not-past t))
  "Returns an interpritation of the numeric value of an
axis or button."
  (let* ((raw-val (if not-past
		      (get-key-state key :current-state keymap)
		    (get-key-state key :prev-state keymap))))
    (convert-to-numeric raw-val)))

(defun get-past-numeric (key &optional (keymap *keymap*))
  "Returns an interpritation of the numeric value of an
axis or button."
  (let* ((raw-val (get-key-state key :prev-state keymap)))
    (convert-to-numeric raw-val)))

(defun get-change (key)
  (let* ((raw-current-val (get-key-state key :current-state))
	 (current-val (cond
		       ((numberp raw-current-val) raw-current-val)
		       ((not raw-current-val) 0.0)
		       (t 1.0)))
	 (raw-prev-val (get-key-state key :prev-state))
	 (prev-val (cond
		    ((numberp raw-prev-val) raw-prev-val)
		    ((not raw-prev-val) 0.0)
		    (t 1.0))))
    (- current-val prev-val)))

(defun get-butterfly-angle (&optional (keymap *keymap*) (not-past t))
  "The 'butterfly angle' is the angle the stick faces such that
the bottom position of the stick counts as the angle 0, and the
topmost position is the angle PI. The side that the stick rests on
makes no diffrence to this angle, though an x-axis neutral stick will
give an angle of either 0, nil (in the case that both x and y axis
are neutral) or PI.
RETURNS: nil or a value between 0.0 and PI inclusive."
  (let ((x-axis (if (get-held :right keymap not-past) (get-numeric :right keymap not-past) (if (get-held :left keymap not-past) (get-numeric :left keymap not-past) 0.0)))
	(y-axis (if (get-held :up keymap not-past) (- (get-numeric :up keymap not-past)) (if (get-held :down keymap not-past) (get-numeric :down keymap not-past) 0.0))))
   (if (or (not (equal 0.0 x-axis)) (not (equal 0.0 y-axis)))
       (atan x-axis y-axis)
     nil)))

;;Note: This function currently returns the SQUARE of the distance as opposed to the distance itself.
(defun get-axis-dist (&optional (keymap *keymap*) (not-past t))
  "The distance from the center of the stick's axis."
  (let* ((x-axis (if (get-held :right keymap not-past) (get-numeric :right keymap not-past) (if (get-held :left keymap not-past) (get-numeric :left keymap not-past) 0.0)))
	 (y-axis (if (get-held :up keymap not-past) (- (get-numeric :up keymap not-past)) (if (get-held :down keymap not-past) (get-numeric :down keymap not-past) 0.0)))
	 (ret-val (+ (* x-axis x-axis) (* y-axis y-axis))))
    (format t "~&x:~a y:~a" x-axis y-axis)
    (if (<= ret-val 1.0) ret-val 1.0)))

(defun get-in-region (&key (min-butterfly 0.0) (max-butterfly pi) (min-axis-dist 0.0) (max-axis-dist 2.0) (keymap *keymap*))
  "Returns true if the joystick is currently in the joystick region inclusively defined by the four values
passed to this function."
  (and (get-butterfly-angle keymap) (<= min-butterfly (get-butterfly-angle keymap) max-butterfly) (<= min-axis-dist (get-axis-dist keymap) max-axis-dist)))

(defun get-region-entered (&key (min-butterfly 0.0) (max-butterfly pi) (min-axis-dist 0.0) (max-axis-dist 2.0) (keymap *keymap*))
  "Returns true if the joystick region inclusively defined by the four values has been entered in the current frame."
  (and
   (and (get-butterfly-angle keymap) (<= min-butterfly (get-butterfly-angle keymap) max-butterfly) (<= min-axis-dist (get-axis-dist keymap) max-axis-dist))
   (not (and (get-butterfly-angle keymap nil) (<= min-butterfly (get-butterfly-angle keymap nil) max-butterfly) (<= min-axis-dist (get-axis-dist keymap nil) max-axis-dist)))))

(defun region-entered-from-center (&key (min-butterfly 0.0) (max-butterfly pi) (min-axis-dist 0.0) (max-axis-dist 2.0) (keymap *keymap*))
  "Returns true if the joystick region inclusively defined by the four values has been entered in the current frame."
  (and
   (and (get-butterfly-angle keymap) (<= min-butterfly (get-butterfly-angle keymap) max-butterfly) (<= min-axis-dist (get-axis-dist keymap) max-axis-dist))
   (not (<= min-axis-dist (get-axis-dist keymap nil) max-axis-dist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INPUT CONFIGURATIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-PS3-defult-config (controler-num)
  (let* ((pad controler-num)
	 (xa 7)
	 (ya 5)
	 (eastd (get-di east pad))
	 (easta (get-ai 7 pad))
	 (westd (get-di west pad))
	 (westa (get-ai 7 pad :dir -1.0))
	 (northd (get-di north pad))
	 (northa (get-ai 6 pad :dir -1.0))
	 (southd (get-di south pad))
	 (southa (get-ai 6 pad))
	 (cb (get-ji 4 pad))
	 (rt (get-ai 5 pad :dir -1.0 :dead-range 0.001 :sticky-range 0.01)))
    (labels ((make-dir-func (dpad stick)
			    "Creates a function combining the functions of both dpad and stick controls."
			    (λ () (let ((val
					 (convert-to-numeric (or (funcall dpad)
								 (funcall stick)))))
				    (if (equal val 0.0) nil val)))))
     (list
      :a1  (get-ji 2 pad) "Button 1"
      :a2 (get-ji 3 pad) "Button 2"
      :defense (get-ji 0 pad) "Sholder R"
      :dodge (get-ji 5 pad) "Button 8"
      :cancel (λ () (or (funcall easta) (funcall westa))) "Button 3"
      :throttle (get-ai 2 pad :dir -1.0 :dead-range 0.001 :sticky-range 0.01) "Trigger R"
      :retard rt "Trigger L"
      :feint cb "Sholder L"
      :down (make-dir-func southa southd)
      "Down Arrow"
      :up (make-dir-func northa northd)
      "Up Arrow"
      :r-left
      (make-dir-func westa westd)
      "Left Arrow"
      :r-right
      (make-dir-func easta eastd)
      "Right Arrow"))))
	  
(defun make-PS3-alt-config (pad)
	(list
   :a1  (get-ji 0 pad) "Button 1"
   :a2 (get-ji 1 pad) "Button 2"
   :defense (get-ji 3 pad) "NUM9"
   :dodge (get-ji 7  pad) "Button 8"
   :cancel (get-ji 2 pad) "Button 3"
   :down (get-ai 5 pad) "Down Arrow"
   :up (get-ai 5 pad :dir -1) "Up Arrow"
   :r-left (get-ai 4 pad :dir -1) "Left Arrow"
   :r-right (get-ai 4 pad) "Right Arrow"))

(defvar *c1-config*
(make-PS3-alt-config 0))

(defvar *c2-config*
  (let ((lf (get-ai 3 0 :dir -1.0))
	(rf (get-ai 3 0))
	(bf (get-ji 3 0)))
   (list
    :a1  (get-ji 0 0) "Button 1"
    :a2 (get-ji 3 0) "Button 2"
    :defense (get-ji 7 0) "NUM9"
    :dodge (get-ji 2 0) "Button 8"
    :cancel (get-ji 1 0) "Button 3"
    :down (get-ai 2 0) "Down Arrow"
    :up (get-ai 2 0 :dir -1.0) "Up Arrow"
    :r-left ;; (λ ()
;; 	       (and
;; 		(funcall bf)
;; 		(funcall lf)))
    (get-ai 3 0 :dir -1.0)
    "Left Arrow"
    :r-right ;; (λ ()
;; 		(and
;; 		 (funcall bf)
;; 		 (funcall rf)))
    (get-ai 3 0)
    "Right Arrow")))

(defvar *k1-config*
  (list
   :a1 (get-kbi KC_H) "J"
   :a2 (get-kbi KC_J) "K"
   :defense (get-kbi KC_K) "L"
   :dodge (get-kbi KC_U) "I"
   :cancel (get-kbi KC_I) "O"
   :down (get-kbi KC_S) "S"
   :up (get-kbi KC_W) "W"
   :r-left (get-kbi KC_A) "A"
   :r-right (get-kbi KC_D) "D"))

(defvar *k2-config*
  (list
   :a1 (get-kbi KC_L) "J"
   :a2 (get-kbi KC_SEMICOLON) "K"
   :defense (get-kbi KC_APOSTROPHE) "L"
   :dodge (get-kbi KC_O) "I"
   :cancel (get-kbi KC_P) "O"
   :down (get-kbi KC_DOWN) "S"
   :up (get-kbi KC_UP) "W"
   :r-left (get-kbi KC_LEFT) "A"
   :r-right (get-kbi KC_RIGHT) "D"))

(defvar *1p-input*
  *c1-config*)

(defvar *2p-input*
  *k2-config*)