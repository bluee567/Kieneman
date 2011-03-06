(in-package "KIENEMAN")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generic Function Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ia-slot (form)
  "[Initarg/Accessor-slot] Given an argument 'a' expands to a slot
definition providing an initarg of ':a' and and
accessor of 'a'. Additional options can be
specified in the body of the form."
  (let* ((slot (car form))
	 (slot-str (string-upcase (string slot)))) ;Converts slot's symbol to a string so that a ':' can be added.
    `(,slot
    :initarg ,(intern slot-str "KEYWORD")
    :accessor ,slot
    ,@(cdr form))))

(defun iea-slot (form)
  ""
  (let ((slot (car form)))
    (ia-slot `(,@form :initform
		   (error ,(concatenate 'string "Must supply a value for " (string-downcase (string slot)) "."))))))

(defmacro defclass+ (name supers slots &rest meta)
  (let ((final-slots (map-by-car (slot slots)
				  (:ia (ia-slot (cdr slot)))
				  (:iea (iea-slot (cdr slot))))))
   `(progn 
      (eval-always
       (setf (get ',name 'slots) ',(mapcar #'first final-slots))
       (setf (get ',name 'supers) ',supers))
      (defclass ,name ,supers
	,final-slots
	,@meta))))

(defun class-slot-names (class-name)
  "Given a CLASS-NAME, returns a list of the slots in the class."
  (mapcar #'clos:slot-definition-name
          (clos:class-slots (find-class class-name))))


(defmacro defdelegate (parent delegate methods)
  "Accepts a class name (parent) an accesable member of 
the class (delegate) and a list of methods which are
to be called on the delegate member when they are called on
the parent class.

Each method must either consist solely of it's name (in which
case the functions has an argument of the single delegate) or be
a list with its name at the front and with all arguments apart
from the first as the cdr.

NOTE: This is not to be confused with the member function
style delegate concept of other languages."
  `(progn
     ,@(mapcar
      #'(lambda (method)
	  (let ((method (if (listp method) (car method) method))
		(args (if (listp method) (cdr method) ())))
	    `(defmethod ,method ((,parent ,parent) ,@args)
	       (,method (,delegate ,parent) ,@args)))
	  )
      methods)))

;; (defmacro (value test-form)
;;   `(let ((value ,value))
;;      (if ,test-form)))

;(defgeneric class-slot-names (class)) DOESN'T WORK

;(defmethod class-slot-names ((instance standard-object))
;  "Given an INSTANCE, returns a list of the slots in the instance's class."
;  (mapcar #'clos:slot-definition-name
;         (clos:class-slots (class-of instance))))

;Tests

;; (defclass+ test-class ()
;;   ((:iea foo)
;;    (bar :initarg :bar :initform 0)))

;; (defvar *tc1* (make-instance 'test-class :foo "blog"))
;; (foo *tc1*)