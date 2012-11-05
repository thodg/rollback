;;
;;  rollback  -  rollback functions
;;
;;  Copyright 2012 Thomas de Grivel <billitch@gmail.com>
;;
;;  Permission to use, copy, modify, and distribute this software for any
;;  purpose with or without fee is hereby granted, provided that the above
;;  copyright notice and this permission notice appear in all copies.
;;
;;  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;

(defpackage :rollback
  (:use :cl)
  (:export #:rollback-function
	   #:rollback
	   #:with-rollback
	   #:with-rollback*))

(in-package :rollback)

(defun rollback-function (op)
  (get op 'rollback-function))

(defun set-rollback-function (op rollback-fn)
  (setf (get op 'rollback-function) rollback-fn))

(defsetf rollback-function set-rollback-function)

(defun rollback (op &rest args)
  (let ((rollback-fn (rollback-function op)))
    (unless rollback-fn
      (error "Undefined rollback function for ~S" op))
    (apply rollback-fn args)))

(defmacro with-rollback ((fun &rest args) &body body)
  (let ((rollback (gensym "ROLLBACK-"))
	(g!args (mapcar (lambda (x)
			  (declare (ignore x))
			  (gensym "ARG-"))
			args)))
    `(let ((,rollback t)
	   ,@(loop
		for var in g!args
		for value in args
		collect `(,var ,value)))
       (,fun ,@g!args)
       (unwind-protect (prog1 ,(if (= 1 (length body))
				   (car body)
				   `(progn ,@body))
			 (setf ,rollback nil))
	 (when ,rollback
	   (rollback ',fun ,@g!args))))))

(defmacro with-rollback* (&body forms)
  (reduce (lambda (body form)
	    (if body
		`(with-rollback ,form
		   ,body)
		form))
	  (reverse forms)))
