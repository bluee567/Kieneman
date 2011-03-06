(asdf:operate 'asdf:load-op 'cl-store)


(cl-store:store '(1 3 2) "t.txt")

(defvar st (cl-store:restore "t.txt"))

(format t "~a" st)

(defgeneric is-stream (str)
  (:method ((str stream))
	   (format t "Yes!"))
  (:method ((str t))
	   (format t "No!")))

(defgeneric is-bin-stream (str)
  (:method ((str gray:fundamental-binary-output-stream))
	   (format t "Yes!"))
  (:method ((str t))
	   (format t "No!")))

(defclass x1 ()
  ((a :accessor a :initarg :a)
   (b :accessor b :initarg :b)))

(let* ((ca (make-instance 'x1))
       (cb (make-instance 'x1 :a ca))
       (cc (make-instance 'x1 :a ca :b cb)))
  (setf (a ca) cb)
  (setf (b ca) cc)
  (setf (b cb) cc)
  (cl-store:store (list ca cb cc) "t2.txt"))

(setf st (cl-store:restore "t2.txt"))

(format t "~a" st)

st

(with-open-file (str "t2.txt" :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
		(cl-store:store 5 str))

(defclass cust-stream (gray:fundamental-binary-output-stream)
  ((store :accessor store :initarg :store :initform ())))

(defmethod gray:stream-write-byte ((str cust-stream) int)
  (setf (store str) (append (store str)  (cons int ()))))

(defclass cust-in-stream (gray:fundamental-binary-input-stream)
  ((store :accessor store :initarg :store :initform ())))

(defmethod gray:stream-read-byte ((str cust-in-stream))
  (pop (store str)))

(defparameter *cust-str* (make-instance 'cust-stream))
(cl-store:store "ctext" *cust-str*)