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

(defclass buffered-input-stream (trivial-gray-streams:fundamental-character-input-stream)
  ((buffer-size :accessor buffered-input-size
                :initarg  :buffer-size
                :initform 1024)
   (buffer :reader buffered-input-buffer)))


(defvar *m4-string*)
(defvar *m4-macro-queue*)
(defvar *m4-parsing-row*)
(defvar *m4-parsing-column*)

(defun m4-lexer (&optional (peek nil))
  (labels ((dynamic-scan (rules)
             (apply #'values
                    (some #'(lambda (pair)
                              (let ((match (cl-ppcre:scan-to-strings (concatenate 'string "^" (car pair)) *m4-string*)))
                                (when match
                                  (list (cdr pair) match (length match)))))
                          rules))))
    (if *m4-macro-queue*
        (values :macro-token (pop *m4-macro-queue*))
      (multiple-value-bind (class image remainder)
          (dynamic-scan `((,*m4-quote-start* . :quote-start)
                          (,*m4-quote-end* . :quote-end)
                          (,*m4-comment* . :comment)
                          (,*m4-macro-name* . :macro-name)
                          ("\\n" . :newline)
                          ("\\(" . :open-paren)
                          ("\\)" . :close-paren)
                          ("." :token)))
        (when (and remainder (null peek))
          (if (equal :newline class)
              (progn
                (setq *m4-parsing-column* 0)
                (incf *m4-parsing-row*))
            (incf *m4-parsing-column* remainder))
          (setq *m4-string* (subseq *m4-string* remainder)))
        (values class image)))))
