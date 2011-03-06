(in-package "KIENEMAN")

(DEFAULT-FOREIGN-LANGUAGE :stdc)
(default-foreign-library "OgreTut1.dll")

;; (defmacro def-call-out+ (name &body forms)
;;   "Creates a generic function and method so that
;; a CLISP ffi callout can be used with polymorphism."
;;   (let ((arg-list
;; 	 (mapcar
;; 	  #'(lambda (arg) (car arg))
;; 	  (cdar (remove-if-not
;; 		#'(lambda (form) (equal (car form) :arguments))
;; 		forms))))
;; 	(-name (intern (concatenate 'string "-" (string name)))))
    
;;    `(progn
;;       (def-call-out ,-name
;; 	,@forms)

;;       (defgeneric ,name ,arg-list)
      
;;       (defmethod ,name ,arg-list
;; 	(,-name ,@arg-list)))))

;; (defgeneric add-time (animation time))

;; (defmethod add-time (anim time)
;;   (-add-time anim time))

;; (defclass+ animation ()
;;   ())

;;NOTE: Get-animation-state on an entity will yeild an animation state when passed
;;an enity and a name.


(def-call-out+ get-animation-name
  (:name "get_animation_name")
  (:arguments (cptr c-pointer))
  (:return-type c-string))

(def-call-out+ get-time-position
  (:name "get_time_position")
  (:arguments (cptr c-pointer))
  (:return-type single-float))

(def-call-out+ set-time-position
  (:name "set_time_position")
  (:arguments (cptr c-pointer) (timePos Single-Float))
  (:return-type nil))

(def-call-out+ get-length
  (:name "get_length")
  (:arguments (cptr c-pointer))
  (:return-type single-float))

(def-call-out+ set-length
  (:name "set_length")
  (:arguments (cptr c-pointer) (len Single-Float))
  (:return-type nil))

(def-call-out+ get-weight
  (:name "get_weight")
  (:arguments (cptr c-pointer))
  (:return-type single-float))

(def-call-out+ set-weight
  (:name "set_weight")
  (:arguments (cptr c-pointer) (weight Single-Float))
  (:return-type nil))

(def-call-out+ add-time
  (:name "add_time")
  (:arguments (cptr c-pointer) (os Single-Float))
  (:return-type nil))

(def-call-out+ has-ended
  (:name "has_ended")
  (:arguments (cptr c-pointer))
  (:return-type boolean))

(def-call-out+ get-enabled
  (:name "get_enabled")
  (:arguments (cptr c-pointer))
  (:return-type boolean))

(def-call-out+ set-enabled
  (:name "set_enabled")
  (:arguments (cptr c-pointer) (enabled boolean))
  (:return-type nil))

(def-call-out+ set-loop
  (:name "set_loop")
  (:arguments (cptr c-pointer) (l boolean))
  (:return-type nil))

(def-call-out+ get-loop
  (:name "get_loop")
  (:arguments (cptr c-pointer))
  (:return-type boolean))

(def-call-out+ copy-state-from
  (:name "copy_state_from")
  (:arguments (cptr c-pointer) (animState c-pointer))
  (:return-type nil))

(def-call-out+ get-anim-parent
  (:name "get_anim_parent")
  (:arguments (cptr c-pointer))
  (:return-type c-pointer))