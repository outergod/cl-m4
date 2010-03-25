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

(define-condition macro-invocation-condition (fucc:parse-error-condition)
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

(dso-lex:deflexer scan-m4 (:priority-only t)
  (" " :space)
  ("\\n" :newline)
  ("\\(" :open-paren)
  ("\\)" :close-paren)
  ("," :comma)
  ("`" :quote-start)
  ("'" :quote-end)
  ("#" :comment)
  ("dnl" :dnl)
  ("[_a-zA-Z]" :macro-start)
  ("\\w+" :name)
  ("[^ \\n(),`'#]+" :string))

(defvar *m4-quoting-level*)
(defvar *m4-string*)

(defun m4-lexer ()
  #'(lambda ()
      (multiple-value-bind (class image remainder)
          (scan-m4 *m4-string*)
        (when remainder
          (setq *m4-string* (subseq *m4-string* remainder)))
        (values class image))))

(defun m4-call-macro (macro args)
  (format t "macro invocation ~a with args ~s~%" macro args)
  (cerror "Use result of macro invocation" 'macro-invocation-condition :result " ")
  macro)

(fucc:defparser *m4-parser*
  m4 (:open-paren :close-paren :space :newline :comma :quote-start :quote-end :macro-start :name :string :comment :dnl)
  ((m4 = (:var token-list (cl:* token))
       (:do (format nil "~{~a~}" token-list)))
   (token = (:or macro-invocation quoted-string dnl comment string))
   (string = (:or :space :newline :comma :string :dnl :name :open-paren :close-paren))
   (dnl = :dnl :newline
        (:do (if (> *m4-quoting-level* 1)
                 (format nil "dnl~%")
               ""))
        = :dnl (:var first (:or :space :comma :quote-start :quote-end :comment))
               (:var rest (cl:* (:or :open-paren :close-paren :space :comma :quote-start :quote-end :macro-start :name :string :comment :dnl)))
               :newline
        (:do (if (> *m4-quoting-level* 1)
                 (format nil "dnl~a~{~a~}~%" first rest)
               "")))
   (comment = :comment
               (:var rest (cl:* (:or :open-paren :close-paren :space :comma :quote-start :quote-end :macro-start :name :string :comment :dnl)))
               (:var newline :newline)
        (:do (format nil "#~{~a~}~a" rest newline)))
   (quote-start = (:var quote :quote-start)
                (:do (incf *m4-quoting-level*)
                     quote))
   (quote-end = (:var quote :quote-end)
              (:do (decf *m4-quoting-level*)
                   quote))
   (quoted-string = (:var quote-start quote-start)
                    (:var string (cl:* (:or :dnl :string :comment
                                            :macro-start :name
                                            :open-paren :close-paren
                                            :space :newline :comma
                                            quoted-string)))
                    (:var quote-end quote-end)
                  (:do (if (> *m4-quoting-level* 0)
                           (format nil "~a~{~a~}~a" quote-start string quote-end)
                         (format nil "~{~a~}" string))))
   (macro-start = (:var start (:or :dnl :macro-start)) (:var rest (cl:+ (:or :name :macro-start :dnl)))
                (:do (format nil "~a~{~a~}" start rest)))
   (macro-invocation = (:var name macro-start)
                     (:do (m4-call-macro name nil))
                     = (:var name macro-start)
                       :open-paren (:var arguments (cl:* (:or macro-invocation quoted-string dnl comment :space :newline :comma :string :name))) :close-paren
                       (:do (m4-call-macro name (or (mapcar #'(lambda (string)
                                                                  (string-left-trim " " string))
                                                              (split-merge arguments ","))
                                                      ""))))) ; http://www.gnu.org/software/m4/manual/m4.html#Invocation
  :prec-info ((:right :macro-start :dnl :comment)
              (:left :space :newline :comma :string :name :quote-start)
              (:right :open-paren :close-paren))
  :type :lalr)

(defun test-m4 (string)
  (let ((*m4-quoting-level* 0)
        (*m4-string* (copy-seq string)))
    (handler-bind ((macro-invocation-condition
                    #'(lambda (error)
                        (setq *m4-string* (concatenate 'string (macro-invocation-result error) *m4-string*))
                        (invoke-restart 'continue))))
                  (fucc:parser-lr (m4-lexer) *m4-parser*))))
