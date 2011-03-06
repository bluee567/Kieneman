(in-package "KIENEMAN")

;;;State Changes
(defmethod change-state ((f1 fighter) (s state))
  (transition-state f1 (state f1) s))

(defmethod transition-state ((f1 fighter) (old-state state) (new-state state))
  (exit-state f1 old-state)
  (transition-animation f1 old-state new-state)
  (enter-state f1 new-state))

(defmethod enter-state ((f1 fighter) (new-state state))
  (setf (parent new-state) f1)
  (setf (state f1) new-state))

(defmethod exit-state ((f1 fighter) (old-state state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Animations

(defmethod transition-animation ((f1 fighter) (old-anim animation) (new-anim animation))
  (exit-animation f1 old-anim)
  (enter-animation f1 new-anim))

(defmethod transition-animation ((f1 fighter) (old-anim animation) (new-anim combined-animation))
  (setf (prev-animation new-anim) old-anim)
  (enter-animation f1 new-anim))

(defmethod exit-animation ((f1 fighter) (old-anim combined-animation))
  (set-enabled (animation old-anim) nil)
  (when (not (eql (prev-animation old-anim) nil))
   (exit-animation f1 (prev-animation old-anim))))

(defmethod enter-animation ((f1 fighter) (new-state single-animation))
  (setf (animation new-state) (set-animation (ogre-entity f1) (name new-state) t))
  (set-time-position (animation new-state) 0.0))

(defmethod enter-animation ((f1 fighter) (new-state combined-animation))
  (setf (animation new-state) (set-animation (ogre-entity f1) (name new-state) t))
  (set-time-position (animation new-state) 0.0))

(defmethod exit-animation ((f1 fighter) (old-anim single-animation))
  (set-enabled (animation old-anim) nil))
