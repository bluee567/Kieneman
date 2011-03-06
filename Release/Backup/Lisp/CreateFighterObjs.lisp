(in-package "KIENEMAN")

;;(defun )
(defun create-fighters ()
;;;Creates characters and adds them as actors
 (let* ((key-table (list
		    :a1 ()
		    :a2 ()
		    :defense ()
		    :down ()
		    :up ()
		    :r-left ()
		    :r-right ()))
	(key-override nil)
	(set-key-buffer
	 #'(lambda (&rest keys)
	     (loop for k in key-table by #'cddr do (setf (getf key-table k) nil))
	     (dolist (k keys)
	       (setf (getf key-table k) t))))
	(set-key-override
	 #'(lambda (val)
	     (setf key-override val))))
			      
   (labels ((new-func (fun key) ;;Accepts a function and
	      #'(lambda ()
		  (if key-override
		      (getf key-table key)
		      (funcall fun))))
	   
	    (transform-f (key fun name)
	      `(,key (create-input-key (new-func ,fun ,key) ,name)))
	    (list-keys (&rest keys)
	      (apply #'append
		     (loop for (key fun name) on keys by #'cdddr collect (list key (create-input-key (new-func fun key) name))))))
     (setf *p1* (make-instance 'tika :x -50.0 :y 0.0 :name "Tika-P1"
			       :left-leg-pos positive
			       :death-function #'reset
			       :buffered-input-func set-key-buffer
			       :input-override-func set-key-override
			       :input-funcs
			       (list-keys
				:a1 (get-kbi KC_J) "J"
				:a2 (get-kbi KC_K) "K"
				:defense (get-kbi KC_L) "L"
				:down (get-kbi KC_S) "S"
				:up (get-kbi KC_W) "W"
				:r-left (get-kbi KC_A) "A"
				:r-right (get-kbi KC_D) "D")))))

 ;; (let* ((a (list
 ;; 				       :a1 ()
 ;; 				       :a2 ()
 ;; 				       :defense ()
 ;; 				       :down ()
 ;; 				       :up ()
 ;; 				       :r-left ()
 ;; 				       :r-right ()
 ;; 				       ;;Override determines if this list 
 ;; 				       :override nil
 ;; 				       :set-buffer nil)))
 ;; 			      (setf (getf a :set-buffer)
 ;; 				    #'(lambda (&rest keys)
 ;; 					;; (dolist ())
 ;; 					()))
			      
 ;; 			      (flet ((new-fun (fun) ;;Accepts a function and
 ;; 				       #'(lambda ()
 ;; 					   (if (getf a :override)
 ;; 					       (getf a key)
 ;; 					       (fun))))
				     
 ;; 				     (trans ((key fun name))
 ;; 					  `(,key (create-input-key (new-fun ,fun) ,name))))
				
 ;; 				(macrolet ((list-funs (&rest body))
 ;; 					   (cons 'list (mapcar #'trans body)))
 ;; 				  (list-funs
 ;; 					:a1 (create-input-key (get-kbi KC_J) "J")
 ;; 					:a2 (create-input-key (get-kbi KC_K) "K")
 ;; 					:defense (create-input-key (get-kbi KC_L) "L")
 ;; 					:down (create-input-key (get-kbi KC_S) "S")
 ;; 					:up (create-input-key (get-kbi KC_W) "W")
 ;; 					:r-left (create-input-key (get-kbi KC_A) "A")
 ;; 					:r-right (create-input-key (get-kbi KC_D) "D")))))

 (add-actor *p1*)

 (let* ((key-table (list
		    :a1 ()
		    :a2 ()
		    :defense ()
		    :down ()
		    :up ()
		    :r-left ()
		    :r-right ()))
	(key-override nil)
	(set-key-buffer
	 #'(lambda (&rest keys)
	     (loop for k in key-table by #'cddr do (setf (getf key-table k) nil))
	     (dolist (k keys)
	       (setf (getf key-table k) t))))
	(set-key-override
	 #'(lambda (val)
	     (setf key-override val))))
			      
   (labels ((new-func (fun key) ;;Accepts a function and
	      #'(lambda ()
		  (if key-override
		      (getf key-table key)
		      (funcall fun))))
	   
	    (transform-f (key fun name)
	      `(,key (create-input-key (new-func ,fun ,key) ,name)))
	    (list-keys (&rest keys)
	      (apply #'append
		     (loop for (key fun name) on keys by #'cdddr collect (list key (create-input-key (new-func fun key) name))))))
     (setf *p2* (make-instance 'tika :x 50.0 :y 0.0 :name "Tika-P2"
			       :left-leg-pos negative
			       :death-function #'reset
			       :buffered-input-func set-key-buffer
			       :input-override-func set-key-override
			       :input-funcs
			       (list-keys
				:a1  (get-ji 0 0) "NUM7"
				:a2 (get-ji 1 0) "NUM8"
				:defense (get-ji 7 0) "NUM9"
				:down (get-di south 0) "Down Arrow"
				:up (get-di north 0) "Up Arrow"
				:r-left (get-di west 0) "Left Arrow"
				:r-right (get-di east 0) "Right Arrow")))))
 (add-actor *p2*)

;;;Sets each fighter's target (opponent) to the other.
 (setf (target *p1*) *p2*)
 (setf (target *p2*) *p1*))


(create-fighters)