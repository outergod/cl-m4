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

(defvar *m4-quoting-level*)
(defvar *m4-string*)

(defun m4-lexer ()
  (let ((m4-macro-level 0))
    #'(lambda ()
        ;; (if (> m4-macro-level 0)
        ;;     (progn
        ;;       (decf m4-macro-level)
        ;;       (format t "read token [~a] of class [~a]~%" "" :macro-block)
        ;;       (values :macro-block ""))
          (multiple-value-bind (class image remainder)
              (scan-m4 *m4-string*)
            (format t "read token [~a] of class [~a]~%" image class)
            (when remainder
              (setq *m4-string* (subseq *m4-string* remainder)))
            (when (or (equal :macro-name class)
                      (equal :close-paren class))
              (incf m4-macro-level))
            (values class image)))))

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

(fucc:defparser *m4-parser*
  m4 (:open-paren :close-paren :space :newline :comma :quote-start :quote-end :macro-name :token :comment :dnl)
  ((m4 = (:var token-list (cl:* token))
       (:do (format nil "~{~a~}" token-list)))
   (token = (:or macro-invocation quoted-string comment string dnl))
   (string = (:or :space :newline :comma :token :open-paren :close-paren))
   (comment = :comment
               (:var rest (cl:* (:or :open-paren :close-paren :space :comma :quote-start :quote-end :macro-name :token :comment :dnl)))
               (:var newline :newline)
        (:do (format nil "#~{~a~}~a" rest newline)))
   (dnl = (:var dnl :dnl)
          (:var rest (cl:* (:or :open-paren :close-paren :space :comma :quote-start :quote-end :macro-name :token :comment :dnl)))
          :newline
        (:do (if (> *m4-quoting-level* 0)
                 (format nil "~a~{~a~}~%" dnl rest)
               "")))
   (quote-start = (:var quote :quote-start)
                (:do (incf *m4-quoting-level*)
                     quote))
   (quote-end = (:var quote :quote-end)
              (:do (decf *m4-quoting-level*)
                   quote))
   (quoted-string = (:var quote-start quote-start)
                    (:var string (cl:* (:or :token :comment
                                            :macro-name :dnl
                                            :open-paren :close-paren
                                            :space :newline :comma
                                            quoted-string)))
                    (:var quote-end quote-end)
                  (:do (if (> *m4-quoting-level* 0)
                           (format nil "~a~{~a~}~a" quote-start string quote-end)
                         (format nil "~{~a~}" string))))
   (macro-invocation = (:var name :macro-name)
                     (:do (m4-call-macro name nil))
                     = (:var name :macro-name)
                       :open-paren (:var arguments (cl:* (:or dnl macro-invocation quoted-string comment :space :newline :comma :token))) :close-paren
                       (:do (m4-call-macro name (or (mapcar #'(lambda (string)
                                                                  (string-left-trim " " string))
                                                              (split-merge arguments ","))
                                                      ""))))) ; http://www.gnu.org/software/m4/manual/m4.html#Invocation
  :prec-info ((:left :space :newline :comma :token :quote-start)
              (:right :comment :dnl)
              (:right :macro-name :open-paren))
  :type :lalr)
                  
(defun test-m4 (string)
  (let ((*m4-quoting-level* 0)
        (*m4-string* (copy-seq string)))
    (handler-bind ((macro-invocation-condition
                    #'(lambda (error)
                        (setq *m4-string* (concatenate 'string (macro-invocation-result error) *m4-string*))
                        (invoke-restart 'continue))))
                  (fucc:parser-lr (m4-lexer) *m4-parser*))))
