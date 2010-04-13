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

(define-condition macro-invocation-condition (error)
  ((result :initarg :result
           :reader macro-invocation-result)))

(define-condition macro-dnl-invocation-condition (error) ())

(defparameter *m4-lib* (make-hash-table :test #'equal))

(defmacro defm4macro (name args &body body)
  (let ((macro-args (gensym))
        (ignored-rest (gensym)))
    `(setf (gethash ,name *m4-lib*)
           #'(lambda (&rest ,macro-args)
               ,(if (member '&rest args)
                    `(destructuring-bind ,args ,macro-args
                       ,@body)
                 `(destructuring-bind (,@args &rest ,ignored-rest) ,macro-args
                    (when ,ignored-rest
                      (warn (format nil "excess arguments to builtin `~a' ignored~%" ,name)))
                    ,@body))))))

(defun macro-return (result)
  (error 'macro-invocation-condition :result result))

(defm4macro "dnl" ()
  (error 'macro-dnl-invocation-condition))

(defm4macro "define" (name result)
  (setf (gethash name *m4-lib*)
        #'(lambda (&rest macro-args)
            (cl-ppcre:regex-replace-all "\\$(\\d+)" result
                                        (replace-with-region
                                         #'(lambda (match)
                                             (or (nth (1- (parse-integer match)) macro-args) ""))))))
  "")

(defm4macro "ifdef" (name string-1 &optional (string-2 ""))
  (macro-return
   (if (m4-macro name)
       string-1
     string-2)))

(defm4macro "ifelse" (&rest args)
  (labels ((ifelse (string-1 string-2 then &rest else)
             (cond ((string= string-1 string-2)
                    (macro-return then))
                   ((> (list-length else) 1)
                    (apply #'ifelse else))
                   (t (macro-return (car else))))))
    (let ((num-args (list-length args)))
      (cond ((= 0 num-args) "ifelse") ; "The macro ifelse is recognized only with parameters"
            ((= 1 num-args) "")       ; "Used with only one argument, the ifelse simply discards it and produces no output"
            ((= 2 num-args)
             (warn "too few arguments to builtin `ifelse'~%")
             "")
            ((< num-args 5)
             (ifelse (car args) (cadr args) (caddr args) (or (cadddr args) "")))
            ((= 5 num-args)
             (warn "excess arguments to builtin `ifelse' ignored~%")
             (ifelse (car args) (cadr args) (caddr args) (cadddr args)))
            (t (apply #'ifelse (car args) (cadr args) (caddr args) (cdddr args)))))))

(defun m4-macro (macro)
  (gethash macro *m4-lib*))

(defmacro with-m4-lib (&body body)
  `(let ((*m4-lib* (alexandria:copy-hash-table *m4-lib*)))
     ,@body))
