(require "Kieneman.lisp")

(defmacro ein (a)
  `(+ ,a 1))

(defmacro zwei (a-list)
  `(progn
     ,@(mapcar
	#'(lambda (alias) `(ein ,alias))
	a-list)))


(zwei (1 2 3))

(defmacro with-def-c-type (type alias-list)
  "Declares all the symbols in the body to be aliases of
c-type type"
  `(progn
     ,@(mapcar
	#'(lambda (alias) `(ffi:def-c-type ,alias ,type))
	alias-list)))

(with-def-c-type ffi:c-pointer
		  (input-toggle
		   root
		   scene-manager
		   node
		   scene-node
		   entity
		   camera
		   vector3
		   quaternion
		   degree
		   radian
		   viewport
		   light
		   tex-area-overlay-element))

(ffi:def-c-type hin ffi:int)