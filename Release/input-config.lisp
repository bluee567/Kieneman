(in-package "KIENEMAN")

(defun make-lamb (a b)
	#'(lambda () (or (funcall a) (funcall b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INPUT CONFIGURATIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
   :alt-def (get-kbi KC_SLASH) "/"
   :dodge (get-kbi KC_O) "I"
   :move (get-kbi KC_LBRACKET) "["
   :cancel (get-kbi KC_P) "O"
   :down (get-kbi KC_DOWN) "S"
   :up (get-kbi KC_UP) "W"
   :r-left (get-kbi KC_LEFT) "A"
   :r-right (get-kbi KC_RIGHT) "D"))

(defun make-PS3-defult-config (pad)
  (let ((d (get-di south pad)) (d2 (get-ai 2 pad)) (u (get-di north pad)) (u2 (get-ai 2 pad :dir -1))
  (l (get-di west pad)) (l2 (get-ai 3 pad :dir -1)) (r (get-di east pad)) (r2 (get-ai 3 pad)))
   (list
   :a1  (get-ji 3 pad) "Button 1"
   :a2 (get-ji 0 pad) "Button 2"
   :defense (get-ji 5 pad) "b7"
   :alt-def (get-ji 7 pad) "b6"
   :move (get-ji 1 pad) "Button 8"
   :cancel (let ((f1 (get-ji 4 pad)) (f2 (get-ji 6 pad))) #'(lambda () (or (funcall f1) (funcall f2)))) "Button 3"
   :dodge (get-ji 2  pad) "Left Trigger"
   :down (make-lamb d d2) "Down Arrow"
   :up  (make-lamb u u2) "Up Arrow"
   :r-left  (make-lamb l l2) "Left Arrow"
   :r-right  (make-lamb r r2) "Right Arrow")))

(defun make-logitech-config (pad)
  (let ((d (get-di south pad)) (d2 (get-ai 0 pad)) (u (get-di north pad)) (u2 (get-ai 0 pad :dir -1))
  (l (get-di west pad)) (l2 (get-ai 1 pad :dir -1)) (r (get-di east pad)) (r2 (get-ai 1 pad)))
   (list
   :a1  (get-ji 3 pad) "Button 1"
   :a2 (get-ji 0 pad) "Button 2"
   :defense (get-ji 5 pad) "b7"
   :alt-def (get-ji 7 pad) "b6"
   :move (get-ai 4 pad :dir -1) "Button 8"
   :cancel (let ((f1 (get-ji 4 pad)) (f2 (get-ji 6 pad))) #'(lambda () (or (funcall f1) (funcall f2)))) "Button 3"
   :dodge (get-ji 2  pad) "Left Trigger"
   :down (make-lamb d d2) "Down Arrow"
   :up  (make-lamb u u2) "Up Arrow"
   :r-left  (make-lamb l l2) "Left Arrow"
   :r-right  (make-lamb r r2) "Right Arrow")))
	  
(defun make-PS3-alt-config (pad)
	(let ((d (get-di south pad)) (d2 (get-ai 6 pad)) (u (get-di north pad)) (u2 (get-ai 6 pad :dir -1))
  (l (get-di west pad)) (l2 (get-ai 7 pad :dir -1)) (r (get-di east pad)) (r2 (get-ai 7 pad)))
	(list
   :a1  (get-ji 3 pad) "Button 1"
   :a2 (get-ji 0 pad) "Button 2"
   :defense (get-ji 7 pad) "b7"
   :alt-def (get-ji 6 pad) "b6"
   :move (get-ji 2  pad) "Button 8"
   :cancel (let ((f1 (get-ji 4 pad)) (f2 (get-ji 5 pad))) #'(lambda () (or (funcall f1) (funcall f2)))) "Button 3"
   :dodge (get-ji 1  pad) "Left Trigger"
   :down (make-lamb d d2) "Down Arrow"
   :up  (make-lamb u u2) "Up Arrow"
   :r-left  (make-lamb l l2) "Left Arrow"
   :r-right  (make-lamb r r2) "Right Arrow")))

(defvar *c1-config*
 nil)

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
    :r-left ;; (? ()
;; 	       (and
;; 		(funcall bf)
;; 		(funcall lf)))
    (get-ai 3 0 :dir -1.0)
    "Left Arrow"
    :r-right ;; (? ()
;; 		(and
;; 		 (funcall bf)
;; 		 (funcall rf)))
    (get-ai 3 0)
    "Right Arrow")))

(defvar *1p-input*
  ;(make-PS3-defult-config 0)
  (make-logitech-config 0)
  ;*k1-config*
  )

(defvar *2p-input*
  *k2-config*
  ;(make-PS3-alt-config 1)
  )