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

(defgeneric flush-buffer (buffered-input-stream)
  (:method ((stream buffered-input-stream))
    (prog1 (subseq (buffered-input-buffer stream) (buffered-input-position stream))
      (fill-buffer stream))))

(defmethod initialize-instance :after ((stream buffered-input-stream)  &rest initargs)
  (declare (ignore initargs))
  (with-accessors ((size buffered-input-size))
      stream
    (setf (buffered-input-buffer stream)
          (make-array (buffered-input-size stream)
                      :element-type 'character :adjustable nil :fill-pointer size))
    (fill-buffer stream)))

(defgeneric fill-buffer (buffered-input-stream)
  (:method ((stream buffered-input-stream))
    (with-accessors ((buffer buffered-input-buffer))
        stream
      (setf (fill-pointer buffer) (read-sequence buffer (buffered-stream stream))
            (buffered-input-position stream) 0))))

(defmethod trivial-gray-streams:stream-read-char :before ((stream buffered-input-stream))
  (with-accessors ((buffer buffered-input-buffer)
                   (position buffered-input-position))
      stream
    (when (>= position (fill-pointer buffer))
      (fill-buffer stream))))

(defmethod trivial-gray-streams:stream-read-char ((stream buffered-input-stream))
  (with-accessors ((buffer buffered-input-buffer)
                   (position buffered-input-position))
      stream
    (if (= 0 (fill-pointer buffer))
        :eof
      (prog1 (char buffer position)
        (incf position)))))

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
           :initform 0)
   (non-stream-position :accessor lexer-non-stream-position
                        :initform 0)
   (double-buffer :accessor lexer-double-buffer
                  :initform "")))

(defgeneric lexer-unread-sequence (lexer-input-stream seq)
  (:method ((stream lexer-input-stream) seq)
    (with-accessors ((double-buffer lexer-double-buffer)
                     (position lexer-non-stream-position))
        stream
      (setq double-buffer (concatenate 'string seq double-buffer))
      (incf position (length seq)))))

(defmethod flush-buffer ((stream lexer-input-stream))
  (with-accessors ((double-buffer lexer-double-buffer))
      stream
    (let ((buffer-contents (call-next-method)))
      (setq double-buffer (concatenate 'string double-buffer buffer-contents))
      buffer-contents)))

(defgeneric stream-read-token (lexer-input-stream &optional peek)
  (:method :before ((stream lexer-input-stream) &optional (peek nil))
    (declare (ignore peek))
    (when (= 0 (length (lexer-double-buffer stream)))
      (flush-buffer stream)))
  (:method :around ((stream lexer-input-stream) &optional (peek nil))
    (with-accessors ((double-buffer lexer-double-buffer)
                     (position lexer-non-stream-position))
        stream
      (multiple-value-bind (class image)
          (call-next-method)
        (when (and class (null peek))
          (let ((length (length image)))
            (setq double-buffer (subseq double-buffer length))
            (if (>= position length)
                (decf position length)
              (if (equal :newline class)
                  (progn
                    (setf (lexer-column stream) 0)
                    (incf (lexer-row stream)))
                (incf (lexer-column stream) (- length position))))))
        (values class image))))
  (:method ((stream lexer-input-stream) &optional (peek nil))
    (declare (ignore peek))
    (with-accessors ((double-buffer lexer-double-buffer))
        stream
      (labels ((scan (chunk rules)
                 (some #'(lambda (pair)
                           (let ((match (cl-ppcre:scan-to-strings (concatenate 'string "^" (car pair)) double-buffer)))
                             (when match
                               (if (= (length match) (length chunk))
                                   (scan (flush-buffer stream) (list pair))
                                 (list (cdr pair) match)))))
                       rules)))
        (apply #'values (scan double-buffer (lexer-rules stream)))))))

(defclass m4-input-stream (lexer-input-stream)
  ((macro-stack :accessor m4-macro-stack
                :initform nil)))

(defgeneric m4-push-macro (m4-input-stream macro)
  (:method ((stream m4-input-stream) macro)
    (setf (m4-macro-stack stream) macro)))

(defgeneric m4-pop-macro (m4-input-stream)
  (:method ((stream m4-input-stream))
    (pop (m4-macro-stack stream))))

(defmethod stream-read-token :around ((stream m4-input-stream) &optional (peek nil))
  (declare (ignore peek))
  (if (m4-macro-stack stream)
      (values :macro-token (m4-pop-macro stream))
    (call-next-method)))
