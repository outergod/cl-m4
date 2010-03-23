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
  ("[_\\w][_\\w\\d]*" :macro-name)
  ("[^ \\n\\$(),`'#]+" :string))

  ;; ("#[^\\n]*\\n" :comment)
  ;; ("dnl[^\\n]*\\n" :dnl)

(defun m4-lexer (string)
  (let ((start 0))
    #'(lambda ()
        (multiple-value-bind (class image remainder)
          (scan-m4 string start)
          (setq start remainder)
          (values class image)))))

  ;; (let ((tokens (dso-lex:lex-all 'scan-m4 string)))
  ;;   #'(lambda ()
  ;;       (let ((token (pop tokens)))
  ;;         (values (car token) (cdr token))))))

(defun m4-call-macro (macro args)
  (declare (ignore args))
  macro)

(fucc:defparser *m4-parser*
  m4 (:dollar :open-paren :close-paren
      :space :newline :comma :quote-start :quote-end
      :macro-name :string :comment :dnl)
  ((m4 = (:var token-list (cl:* token))
       (:do (format nil "~{~a~}" token-list)))
   (token = string
          = character)
   (character = :space
              = :newline
              = :comma
              = :dollar)
   (string = macro-invocation
           = quoted-string
           = dnl
           = :comment
           = :string)
   (dnl = :dnl (:var first (:or character :string :quote-start :quote-end))
               (:var rest (cl:* (:or :dnl :string :comment
                                     :macro-name :open-paren :close-paren
                                     :quote-start :quote-end
                                     character)))
               (:var newline (:maybe :newline))
        (:do (if (> *m4-quoting-level* 1)
                 (format nil "dnl~a~{~a~}~a" first rest newline)
               "")))
   (comment = :comment
               (:var rest (cl:* (:or :dnl :string :comment
                                     :macro-name :open-paren :close-paren
                                     :quote-start :quote-end
                                     character)))
               (:var newline (:maybe :newline))
        (:do (if (> *m4-quoting-level* 1)
                 (format nil "#~{~a~}~a" rest newline)
               "")))
   (quote-start = (:var quote :quote-start)
                (:do (incf *m4-quoting-level*)
                     quote))
   (quote-end = (:var quote :quote-end)
              (:do (decf *m4-quoting-level*)
                   quote))
   (quoted-string = quote-start (:var string (cl:* (:or :dnl :string :comment
                                                        :macro-name :open-paren :close-paren
                                                        character quoted-string))) quote-end
                  (:do (format t "[~d] quoted string ~s~%" *m4-quoting-level* string)
                       (if (> *m4-quoting-level* 1)
                           (format nil "`~a'" string)
                         string)))
   (macro-invocation = (:var name :macro-name)
                       (:var arguments (:maybe :open-paren
                                               (:maybe (:list string :comma))
                                               :close-paren))
                       (:do (progn
                              (format t "macro invocation ~s with args ~s~%" name arguments)
                              (m4-call-macro name arguments)))))
  :type :lalr)

(defun test-m4 (string)
  (let ((*m4-quoting-level* 0))
    (fucc:parser-lr (m4-lexer string) *m4-parser*)))
