(in-package "KIENEMAN")

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

(defun make-PS3-defult-config (controler-num)
  (if t ;(> (no-of-joysticks) controler-num)
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
			    (? () (let ((val
					 (convert-to-numeric (or (funcall dpad)
								 (funcall stick)))))
				    (if (equal val 0.0) nil val)))))
     (list
      :a1  (get-ji 2 pad) "Button 1"
      :a2 (get-ji 3 pad) "Button 2"
      :defense (get-ji 0 pad) "Sholder R"
      :dodge (get-ji 5 pad) "Button 8"
      :cancel (? () (or (funcall easta) (funcall westa))) "Button 3"
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
      "Right Arrow")))
	  (error "Not enough controlers are connected!")))
	  
(defun make-PS3-alt-config (pad)
	(list
   :a1  (get-ji 3 pad) "Button 1"
   :a2 (get-ji 0 pad) "Button 2"
   :defense (get-ji 7 pad) "b7"
   :alt-def (get-ji 6 pad) "b6"
   :move (get-ji 2  pad) "Button 8"
   :cancel (let ((f1 (get-ji 4 pad)) (f2 (get-ji 5 pad))) #'(lambda () (or (funcall f1) (funcall f2)))) "Button 3"
   :dodge (get-ji 1  pad) "Left Trigger"
   :down (get-ai 6 pad) "Down Arrow"
   :up (get-ai 6 pad :dir -1) "Up Arrow"
   :r-left (get-ai 7 pad :dir -1) "Left Arrow"
   :r-right (get-ai 7 pad) "Right Arrow"))

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
  (make-PS3-alt-config 0)
  ;*k1-config*
  )

(defvar *2p-input*
  *k2-config*)