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

(dso-lex:deflexer scan-m4 (:priority-only t)
  (" " :space)
  ("\\n" :newline)
  ("\\$" :dollar)
  ("\\(" :open-paren)
  ("\\)" :close-paren)
  ("," :comma)
  ("`" :quote-start)
  ("'" :quote-end)
  ("\\d+" :integer)
  ("[_\\w][_\\w\\d]*" :macro-name)
  ("[^ \\n\\$(),`']+" :string))

(defun m4-lexer (string)
  (let ((tokens (dso-lex:lex-all 'scan-m4 string)))
    #'(lambda ()
        (let ((token (pop tokens)))
          (values (car token) (cdr token))))))

(fucc:defparser *m4-parser*
  m4 (:integer :dollar :open-paren :close-paren
      :space :newline :comma :quote-start :quote-end
      :macro-name :string)
  ((m4 = (:var token-list (cl:* token))
       (:do (format t "token-list is ~s~%" token-list)))
   (token = string
          = :space
          = :newline
          = :comma
          = :dollar)
   (string = macro-invocation
           = quoted-string
           = :string
           = :integer)
   (quoted-string = :quote-start (:var string (:maybe token)) :quote-end
                  (:do (progn
                         (format t "quoted string ~s~%" string)
                         string)))
   (macro-invocation = (:var name :macro-name)
                       (:var arguments (:maybe :open-paren
                                               (:maybe (:list string :comma))
                                               :close-paren))
                       (:do (progn
                              (format t "macro invocation ~s with args ~s~%" name arguments)
                              (list name arguments)))))
  :type :lalr)

(defun test-m4 (string)
  (fucc:parser-lr (m4-lexer string) *m4-parser*))
