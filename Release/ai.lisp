(in-package "KIENEMAN")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AI CONTROLERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-dummy-ai (fighter pad)
	(let* ((btext "dummy-ai")
		  )
	
	(list
   :a1  (get-ji 3 pad) btext
   :a2 (get-ji 0 pad) btext
   :defense (get-ji 7 pad) btext
   :dodge (get-ji 2  pad) btext
   :cancel (let ((f1 (get-ji 4 pad)) (f2 (get-ji 5 pad))) #'(lambda () (or (funcall f1) (funcall f2)))) btext
   :slow (get-ji 6  pad) btext
   :down (get-ai 6 pad) btext
   :up (get-ai 6 pad :dir -1) btext
   :r-left (get-ai 7 pad :dir -1) btext
   :r-right (get-ai 7 pad) btext)))