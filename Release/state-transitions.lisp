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

(defmethod exit-state ((f1 fighter) (old-state state))
	())
