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

(defclass buffered-input-stream (trivial-gray-streams:fundamental-character-input-stream)
   ((stream :initarg :stream
            :reader buffered-stream
            :initform (alexandria:required-argument :stream))
    (buffer-size :initarg :buffer-size
                 :accessor buffered-input-size
                 :initform 1024)
    (buffer-position :accessor buffered-input-position
                     :initform 0)
    (buffer :accessor buffered-input-buffer)))

(defmethod close ((stream buffered-input-stream) &key abort)
  (close (buffered-stream stream) :abort abort))

(defmethod initialize-instance :after ((stream buffered-input-stream)  &rest initargs)
  (declare (ignore initargs))
  (with-accessors ((size buffered-input-size))
      stream
    (setf (buffered-input-buffer stream)
          (make-array (buffered-input-size stream)
                      :element-type 'character :adjustable nil :fill-pointer size)
          (buffered-input-position stream) size)))

(defmethod trivial-gray-streams:stream-read-char ((stream buffered-input-stream))
  (with-accessors ((buffer buffered-input-buffer)
                   (position buffered-input-position))
      stream
    (when (= position (fill-pointer buffer))
      (setf (fill-pointer buffer) (read-sequence buffer (buffered-stream stream))
            (buffered-input-position stream) -1))
    (if (= 0 (fill-pointer buffer))
        :eof
      (char buffer (incf position)))))

(defmethod trivial-gray-streams:stream-unread-char ((stream buffered-input-stream) char)
  (error "Unreading chars is not supported for buffered-input-streams"))

(defmethod trivial-gray-streams:stream-read-sequence ((stream buffered-input-stream) seq start end &key)
  (read-sequence seq stream :start start :end end))


(defclass lexer-input-stream (buffered-input-stream)
  ((rules :initarg :rules
          :accessor lexer-rules
          :initform nil)
   (row   :accessor lexer-row
          :initform 1)
   (column :accessor lexer-column
           :initform 0)))

(defmethod trivial-gray-streams:stream-read-char ((stream lexer-input-stream))
  (let ((char (call-next-method)))
    (if (equal #\newline char)
        (progn
          (setf (lexer-column stream) 0)
          (incf (lexer-row stream)))
      (incf (lexer-column stream)))
    char))

(defgeneric stream-read-token (lexer-input-stream &optional peek)
  (:method ((lexer lexer-input-stream) &optional (peek nil))
    (labels ((dynamic-scan (rules)
               (apply #'values
                      (some #'(lambda (pair)
                                (let ((match (cl-ppcre:scan-to-strings (concatenate 'string "^" (car pair))
                                                                       (buffered-input-buffer stream))))
                                  (when match
                      (list (cdr pair) match (length match)))))
                            (lexer-rules stream)))))
      (multiple-value-bind (class image remainder)
          (when (and remainder (null peek))
            (if (equal :newline class)
                (progn
                  (setf (lexer-column stream) 0)
                  (incf (lexer-row stream)))
              (incf (lexer-column stream) remainder))
            (incf (buffered-input-buffer stream) remainder))
        (values class image)))))

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
