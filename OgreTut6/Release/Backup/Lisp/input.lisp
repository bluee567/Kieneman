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
  (:return-type int))

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
  (λ (= (boole boole-and (return-dir) direction) direction)))

(defun update-input-interface (interface)
  (loop for key in interface
	when (listp key) do 
	(progn (setf (getf key :prev-state) (getf key :current-state))
	       (setf (getf key :current-state) (funcall (getf key :func))))))

(abbrev get-kbi get-kb-input-func)
(abbrev get-ji get-joy-input-func)
(abbrev get-di get-direction-input-func)

;A keylist of interface functions.
(defvar *input-1*
  (list
   :a1 (list :func (get-kbi KC_A) :label "nil")
   :a2 t
   :defense t
   :down t
   :up t
   :left t
   :right t))

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

(defun get-key-state (key state-key)
  "
NOTE: keymap is a dynamic variable and hence a keymap
variable must be lexicaly bound at the point of invocation."
  (flet ((call-func (sym)
		    (getf (getf *keymap* sym) state-key)))
   (cond
    ((eql key :left)
     (and (not (call-func :r-right))
	  (call-func :r-left)))

    ((eql key :right)
     (and (call-func :r-right)
	  (not (call-func :r-left))))
   
    ((eql key :h-double)
     (and (call-func :r-right)
	  (call-func :r-left)))
    ((eql key :h-nothing)
     (not (or (call-func :r-right)
	      (call-func :r-left))))

    ((eql key :h-neutral)
     (mor get-held (:h-double) (:h-nothing))) ;Yes, it's completly intentional ;)
   
    (t (getf (getf *keymap* key) state-key)))))

(defun get-held (key)
  "Calls the corresponding 'held' func from the current keymap."
  (get-key-state key :current-state))

(defun get-pressed (key)
  (and
   (get-key-state key :current-state)
   (not (get-key-state key :prev-state))))

(defun get-released (key)
  (and
   (get-key-state key :prev-state)
   (not (get-key-state key :current-state))))

(defun get-label (key)
  "Gets the lable of the key in the current keymap.
NOTE: keymap is a dynamic variable and hence a keymap
variable must be lexicaly bound at the point of invocation."
  (getf (getf *keymap* key) :label))


;;(load "ffiogre.lisp")

