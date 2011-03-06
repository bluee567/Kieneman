(in-package "KIENEMAN")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Helper Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro λ (&rest forms)
  "Lambda Literal: Constructs a lambda form with no args"
  `#'(lambda () ,@forms))

(defmacro λx (&rest forms)
  "Lambda literal with x: Constructs a lambda form with x as an arg"
  `#'(lambda (x) ,@forms))

(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))

(defmacro map-by-car ((element list-name) &body cases)
  "Expands to a form which accepts a list of elements, each of which
must be a list (car must be a valid operator on each element). After a two element
form containing the names of the list and the alias each individual element
is to be refered to, the body of the main form must contain a list of cases,
each consisting of a form who's car must be a value which is to
be tested against the car of each list element and who's cdar must be a form which
when evaluates to the value the list element is to be replaced by.
The value returned by the evaluation of the form is the list passed in via 
list name, except that each element who's car matches (is eql to) the car
of one of the cases in the body will be replaced by the result of evaluating
the second atom of the case form."
  `(mapcar
   #'(lambda (,element)
       (case (car ,element)
	 ,@cases
	 (otherwise ,element)))
   ,list-name))

(defmacro var (variable value-form &rest body)
  "A form for when you simply want to 'let' a single variable.
i.e. (var x 10 (var y 20 (+ x y))) evaluates to 30."
  `(let ((,variable ,value-form))
     ,@body))

(var x 10 (var y 20 (+ x y)))

(defun marg (func args)
  "Conses func to the front of each list in args."
  (mapcar
   #'(lambda (arg) (cons func arg))
   args))

(defmacro mor (func &rest args)
  "Expands to multiple calls of a function
each onto an individual list of arguments. The 
return values of these function calls are ored"
  `(or ,@(marg func args)))

(defmacro mand (func &rest args)
  "Expands to multiple calls of a function
each onto an individual list of arguments. The 
return values of these function calls are anded"
  `(and ,@(marg func args)))

;;Old test
;;(mand = (0 0) (1 1) (2 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom Math
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun diff (a b)
  "The magnitude of the diffrence between a and b"
  (abs (- a b)))

(defun mapratio (vlist)
  "Accepts a list of numbers
returns a list of the proportion of each
number compared to the total of all the numbers."
  (var total (loop for x in vlist sum x)
       (mapcar
	(λx (/ x total))
	vlist)))

;;(load "infix.lisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Polymorphic FFI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro def-call-out+ (name &body forms)
  "Creates a generic function and method so that
a CLISP ffi callout can be used with polymorphism."
  (let ((arg-list
	 (mapcar
	  #'(lambda (arg) (car arg))
	  (cdar (remove-if-not
		#'(lambda (form) (equal (car form) :arguments))
		forms))))
	(-name (intern (concatenate 'string "-" (string name)))))
    
   `(progn
      (def-call-out ,-name
	,@forms)

      (defgeneric ,name ,arg-list)
      
      (defmethod ,name ,arg-list
	(,-name ,@arg-list)))))



(defun premutations (list)
  "Given a list, returns all premutations of that list."
  t)

(defmacro defpremethod (name args &body body)
  "Defines a series of methods, each of which contain a diffrent
premutations of all the possible orders of the arguments."
  `(progn
     (defmethod ,name ,args
       ,@body)
     (defmethod ,name (,(cadr args) ,(car args))
       ,@body)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hash Tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro hash (&rest rest)
  "Cosntructs a hash table and accepts key value pairs."
  (let ((hname (gensym))
        (pairs nil))
    (dolist (i rest)
      (push `(setf (gethash ,(car i) ,hname) ,(cadr i))
            pairs))
    `(let ((,hname (make-hash-table)))
       ,@pairs
       ,hname)))

(defun hash-m (&rest pairs)
  (let ((h (make-hash-table :test 'equal)))
    (loop for (key value) on pairs by #'cddr do (setf (gethash key h) value))
    h))


;; loop for (a b) on '(:a :b :c :d :e 3) by #'cddr do (format t "~a-~a " a b))