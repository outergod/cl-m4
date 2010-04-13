;;;; evol - m4-builtin.lisp
;;;; Copyright (C) 2010  Alexander Kahl <e-user@fsfe.org>
;;;; This file is part of evol.
;;;; evol is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; evol is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :evol)

(define-condition macro-dnl-invocation-condition (error) ())

(defvar *m4-lib*)

(defmacro defm4macro (name args &body body)
  (let ((macro-args (gensym))
        (ignored-rest (gensym)))
    `(setf (gethash ,name *m4-lib*)
           #'(lambda (&rest ,macro-args)
               (destructuring-bind (,@args &rest ,ignored-rest) ,macro-args
                 (when ,ignored-rest
                   (format *error-output* "excess arguments to builtin `~a' ignored~%" ,name))
                 ,@body)))))

(defun m4-macro (macro)
  (gethash macro *m4-lib*))

(defmacro with-m4-lib (&body body)
  `(let ((*m4-lib* (make-hash-table :test #'equal)))
     (defm4macro "dnl" ()
       (error 'macro-dnl-invocation-condition))
     (defm4macro "define" (name result)
       (setf (gethash name *m4-lib*)
             #'(lambda (&rest macro-args)
                 (declare (ignore macro-args))
                   result))
       "")
     ,@body))
