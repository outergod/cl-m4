;;;; evol - m4-parser.lisp
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
(use-package '#:dso-parse)

(define-condition macro-invocation-condition (error)
  ((result :initarg :result
           :reader macro-invocation-result)))

(defun split-merge (string-list split-string)
  (labels ((acc (rec string rest)
                (let ((char (car rest)))
                  (cond ((null char) (nreverse (cons string rec)))
                        ((string= split-string (car rest))
                         (acc (cons string rec) "" (cdr rest)))
                        (t (acc rec (concatenate 'string string char) (cdr rest)))))))
          (acc (list) "" string-list)))

(defvar *m4-quoting-level*)
(defvar *m4-string*)

(defun m4-call-macro (macro args)
  (format t "macro invocation ~a with args ~s~%" macro args)
  (cond
   ((string= "dnl" macro)
    (progn
      (let ((position (search (string #\newline) *m4-string*)))
        (setq *m4-string*
              (subseq *m4-string*
                      (if position
                          (1+ position)
                        (length *m4-string*)))))
      ""))
    ((string= "format" macro)
     (cerror "Use result of macro invocation" 'macro-invocation-condition :result "$")
     "")
    (t macro)))
                  
(dso-lex:deflexer scan-m4 (:priority-only t)
  (" " :space)
  ("\\n" :newline)
  ("\\(" :open-paren)
  ("\\)" :close-paren)
  ("," :comma)
  ("`" :quote-start)
  ("'" :quote-end)
  ("#" :comment)
  ("dnl[^\\w]" :dnl)
  ("[_a-zA-Z]\\w+" :macro-name)
  ("[^ \\n(),`'#]" :token))

(defgrammar ()
  (m4 (* token))
  (token (/ comment m4-string newline))
  (comment (#\# (^ "[^\\n]*\\n")) :filter #'(lambda (s)
                                           (format t "~a~%" s)))
  (m4-string (^ "."))
  (newline (^ "\\n")))




  (file2 (+ row2))
  (row2 (value (* row-rest2) (= newline))
       :filter (lambda (row) (cons (car row) (mapcar #'second (second row)))))
  (row-rest2 ((= comma) value) :filter 'identity)
  (squoted-value ((= #\') (^ "([^']|'')+") (= #\'))
                 :cclass t
                 :filter
                 (lambda (s) (cl-ppcre:regex-replace-all "''" (car s) "'")))
  (dquoted-value ((= #\") (^ "([^\"]|\"\")+") (= #\"))
                 :cclass t
                 :filter
                 (lambda (s) (cl-ppcre:regex-replace-all "\"\"" (car s) "\"")))
  (unquoted-value (^ "[^,'\"\\n\\r]+") :cclass t)
  (value (/ squoted-value dquoted-value unquoted-value) :cclass t)
  (newline (^ "\\r\\n?|\\n"))
  (comma #\,))


(defun test-m4 (string)
  (let ((*m4-quoting-level* 0)
        (*m4-string* (copy-seq string)))
    (handler-bind ((macro-invocation-condition
                    #'(lambda (error)
                        (setq *m4-string* (concatenate 'string (macro-invocation-result error) *m4-string*))
                        (invoke-restart 'continue))))
                  (m4 *m4-string*))))
