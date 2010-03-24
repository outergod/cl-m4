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

(defvar *m4-quoting-level*)

(defun split-merge (string-list split-string)
  (labels ((acc (rec string rest)
                (let ((char (car rest)))
                  (cond ((null char) (nreverse rec))
                        ((string= split-string (car rest))
                         (acc (cons string rec) "" (cdr rest)))
                        (t (acc rec (concatenate 'string string char) (cdr rest)))))))
          (acc (list) "" string-list)))

(dso-lex:deflexer scan-m4 (:priority-only t)
  (" " :space)
  ("\\n" :newline)
  ("\\$" :dollar)
  ("\\(" :open-paren)
  ("\\)" :close-paren)
  ("," :comma)
  ("`" :quote-start)
  ("'" :quote-end)
  ("#" :comment)
  ("dnl" :dnl)
  ("[_a-zA-Z]" :macro-start)
  ("\\w+" :name)
  ("[^ \\n\\$(),`'#]+" :string))

(defun m4-lexer (string)
  (let ((start 0))
    #'(lambda ()
        (multiple-value-bind (class image remainder)
          (scan-m4 string start)
          (setq start remainder)
          ;(format t "processing token ~a ~a~%" class image)
          (values class image)))))

(defun m4-call-macro (macro args)
  (format t "macro invocation ~a with args ~s~%" macro args)
  macro)

(fucc:defparser *m4-parser*
  m4 (:dollar :open-paren :close-paren :space :newline :comma :quote-start :quote-end :macro-start :name :string :comment :dnl)
  ((m4 = (:var token-list (cl:* token))
       (:do (format nil "~{~a~}" token-list)))
   (token = (:or macro-invocation quoted-string dnl comment string))
   (string = (:or :dollar :space :newline :comma :string :dnl :name))
   (dnl = :dnl :newline
        (:do (if (> *m4-quoting-level* 1)
                 (format nil "dnl~%")
               ""))
        = :dnl (:var first (:or :space :comma :dollar :quote-start :quote-end :comment))
               (:var rest (cl:* (:or :dollar :open-paren :close-paren :space :comma :quote-start :quote-end :macro-start :name :string :comment :dnl)))
               :newline
        (:do (if (> *m4-quoting-level* 1)
                 (format nil "dnl~a~{~a~}~%" first rest)
               "")))
   (comment = :comment
               (:var rest (cl:* (:or :dollar :open-paren :close-paren :space :comma :quote-start :quote-end :macro-start :name :string :comment :dnl)))
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
                                            :space :newline :comma :dollar
                                            quoted-string)))
                    (:var quote-end quote-end)
                  (:do (if (> *m4-quoting-level* 0)
                           (format nil "~a~{~a~}~a" quote-start string quote-end)
                         (format nil "~{~a~}" string))))
   (macro-start = (:var start (:or :dnl :macro-start)) (:var rest (cl:+ (:or :name :macro-start :dnl)))
                (:do (format nil "~a~{~a~}" start rest)))
   (macro-invocation = (:var name macro-start)
                       (:var arguments (:maybe :open-paren (cl:* token) :close-paren))
                       (:do (m4-call-macro name (when arguments
                                                  (or (split-merge (cadr (butlast arguments)) ",")
                                                      "")))))) ; http://www.gnu.org/software/m4/manual/m4.html#Invocation
  :prec-info ((:right :macro-start :dnl :comment)
              (:left :dollar :space :newline :comma :string :name :quote-start))
  :type :lalr)


                       ;; (:var arguments (:maybe :open-paren
                       ;;                         (:maybe (:list (cl:* token) :comma))
                       ;;                         :close-paren))
                       ;; (:do (m4-call-macro name (when arguments
                       ;;                            (or (caadr (butlast arguments))
                       ;;                                "")))))) ; http://www.gnu.org/software/m4/manual/m4.html#Invocation

(defun test-m4 (string)
  (let ((*m4-quoting-level* 0))
    (fucc:parser-lr (m4-lexer string) *m4-parser*)))

;(list "%d ?" " " "?" " " " " " " "?" "," " " ",")
