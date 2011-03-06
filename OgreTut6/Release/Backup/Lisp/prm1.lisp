;;; -*- Mode: Lisp -*-
;;;
;;; $Header: /home/gene/library/website/docsrc/prm/RCS/prm1.lisp,v 395.1 2008/04/20 17:25:47 gene Exp $
;;;
;;; Copyright (c) 2006 Gene Michael Stover.  All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 
;;; - Redistributions of source code must retain the above copyright
;;; notice, this list of conditions and the following disclaimer.
;;; 
;;; - Redistributions in binary form must reproduce the above copyright
;;; notice, this list of conditions and the following disclaimer in the
;;; documentation and/or other materials provided with the
;;; distribution.
;;; 
;;; - Neither the name Gene Michael Stover nor the names of its
;;; contributors may be used to endorse or promote products derived
;;; from this software without specific prior written permission.
;;; 
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;;; COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;;; OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(defpackage "COM.CYBERTIGGYR.PRM"
  (:use "COMMON-LISP"))
(in-package "COM.CYBERTIGGYR.PRM")

(export 'make-permutator)
(export 'maperm)

(defun maperm (fn &rest lists)
  (cond ((endp lists) nil)
        ((endp (rest lists)) (mapc fn (first lists)))
        (t (mapc #'(lambda (x)
                     (apply #'maperm
                            #'(lambda (&rest args)
                                (apply fn x args))
                            (rest lists)))
                 (first lists))))
  (reduce #'* (mapcar #'length lists)))

(defun make-permutator (&rest lists)
  (cond ((endp lists) #'(lambda () nil))
        ((endp (rest lists)) (let ((lst (first lists)))
                               #'(lambda ()
                                   (and lst
                                        (let ((x (first lst)))
                                          (setq lst (rest lst))
                                          (list x))))))
        (t (let ((lst (first lists))
                 (prm (apply #'make-permutator (rest lists))))
             #'(lambda ()
                 (let ((y (funcall prm)))
                   (cond (y (cons (first lst) y))
                         ;; PRM is done, so we increment LST.
                         ((null (setq lst (rest lst)))
                          ;; We incremented LST, & it's NIL, so we
                          ;; are done.
                          nil)
                         (t 
                          (setq prm (apply #'make-permutator (rest lists)))
                          (cons (first lst) (funcall prm))))))))))

(defun make-test-lists (count)
  (declare (type integer count))
  (assert (plusp count))
  (let ((lst (loop for i from 1 to 3 collect i)))
    (loop for j from 1 to count collect lst)))

(defun time-maperm (count)
  (let ((calls 0))
    (time (apply #'maperm
                 #'(lambda (&rest args)
                     (declare (ignore args))
                     (incf calls))
                 (make-test-lists count)))
  calls))

(defun time-permutator (count)
  (let ((calls 0))
    (time (let ((prm (apply #'make-permutator (make-test-lists count))))
            (do ((x (funcall prm) (funcall prm)))
                ((null x))
                (incf calls))))
    calls))

;;; --- end of file ---
