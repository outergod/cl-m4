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

(defstruct (macro-token (:constructor make-macro-token (m4macro name)))
  m4macro name)

(define-condition macro-invocation-condition (error)
  ((result :initarg :result
           :reader macro-invocation-result)))

(define-condition macro-dnl-invocation-condition (error) ())

(define-condition macro-defn-invocation-condition (error)
  ((macros :initarg :macros
           :reader macro-defn-invocation-result)))

(defparameter *m4-lib* (make-hash-table :test #'equal))
(defvar *m4-runtime-lib*)
(defvar *m4-quote-start*)
(defvar *m4-quote-end*)
(defvar *m4-comment*)
(defvar *m4-macro-name*)

(defun pushm4macro (name fun &optional (replace t))
  (unless (gethash name *m4-runtime-lib*)
    (setf (gethash name *m4-runtime-lib*)
          (make-array 1 :adjustable t :fill-pointer 1)))
  (let ((stack (gethash name *m4-runtime-lib*)))
    (if replace
        (setf (aref stack (1- (fill-pointer stack))) fun)
      (vector-push-extend fun stack))))

(defun popm4macro (name)
  (let ((stack (gethash name *m4-runtime-lib*)))
    (when stack
      (if (> (fill-pointer stack) 1)
          (vector-pop stack)
        (remhash name *m4-runtime-lib*)))))

(defmacro defm4macro (name args &body body)
  (let ((macro-args (gensym))
        (ignored-rest (gensym)))
    `(setf (gethash ,name *m4-lib*)
           (make-array 1 :adjustable t :fill-pointer 1
                         :initial-contents
                         (list #'(lambda (&rest ,macro-args)
                                   ,(if (member '&rest args)
                                        `(destructuring-bind ,args ,macro-args
                                           ,@body)
                                      `(destructuring-bind (,@args &rest ,ignored-rest) ,macro-args
                                         (when ,ignored-rest
                                           (warn (format nil "excess arguments to builtin `~a' ignored~%" ,name)))
                                         ,@body))))))))

(defun defm4runtimemacro (name expansion &optional (replace t))
  (let ((fun (if (macro-token-p expansion)
                 (macro-token-m4macro expansion)
               #'(lambda (&rest macro-args)
                   (macro-return
                    (cl-ppcre:regex-replace-all "\\$(\\d+|#|\\*|@)" expansion
                                                (replace-with-region
                                                 #'(lambda (match)
                                                     (cond ((string= "#" match)
                                                            (write-to-string (length macro-args)))
                                                           ((string= "*" match)
                                                            (format nil "~{~a~^,~}" macro-args))
                                                           ((string= "@" match)
                                                            (format nil
                                                                    (concatenate 'string "~{" *m4-quote-start* "~a" *m4-quote-end* "~^,~}")
                                                                    macro-args))
                                                           (t (let ((num (parse-integer match)))
                                                                (if (= 0 num)
                                                                    name
                                                                  (or (nth (1- num) macro-args) "")))))))))))))
    (pushm4macro name fun replace)))

(defun macro-return (result)
  (error 'macro-invocation-condition :result result))

(defun m4-macro (macro)
  (let ((stack (gethash macro *m4-runtime-lib*)))
    (when stack
      (aref stack (1- (fill-pointer stack))))))

(defmacro with-m4-lib (&body body)
  `(let ((*m4-runtime-lib* (alexandria:copy-hash-table *m4-lib*)))
     ,@body))

(defm4macro "dnl" ()
  (error 'macro-dnl-invocation-condition))

(defm4macro "define" (name &optional (expansion ""))
  (prog1 "" (defm4runtimemacro name expansion)))

(defm4macro "undefine" (&rest args)
  (if (= 0 (list-length args)) ; "The macro undefine is recognized only with parameters"
      "undefine"
    (progn
      (mapc #'(lambda (name)
                (remhash name *m4-runtime-lib*))
            args)
      "")))

(defm4macro "defn" (&rest args)
  (if (= 0 (list-length args)) ; Figured out by trial-and-error
      "defn"
    (error 'macro-defn-invocation-condition
           :macros (mapcar #'(lambda (name)
                               (if (m4-macro name)
                                   (make-macro-token (m4-macro name)
                                                     (if (gethash name *m4-lib*)
                                                         ""
                                                       name))
                                 ""))
                           args))))

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
            ((= 5 num-args)           ; If called with three or four arguments...A final fifth argument is ignored, after triggering a warning
             (warn "excess arguments to builtin `ifelse' ignored~%")
             (ifelse (car args) (cadr args) (caddr args) (cadddr args)))
            (t (apply #'ifelse (car args) (cadr args) (caddr args) (cdddr args)))))))

(defm4macro "shift" (&rest args)
  (if (= 0 (list-length args)) ; "The macro shift is recognized only with parameters"
      "shift"
    (macro-return (format nil (concatenate 'string "~{" *m4-quote-start* "~a" *m4-quote-end* "~^,~}") (cdr args)))))

(defm4macro "pushdef" (name &optional (expansion ""))
  (prog1 "" ; "The expansion of both pushdef and popdef is void"
    (defm4runtimemacro name expansion nil)))

(defm4macro "popdef" (&rest args)
  (if (= 0 (list-length args)) ; "The macros pushdef and popdef are recognized only with parameters"
      "popdef"
    (prog1 "" ; "The expansion of both pushdef and popdef is void"
      (mapc #'popm4macro args))))
