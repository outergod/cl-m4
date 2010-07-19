;;;; cl-m4 - m4-lexer.lisp
;;;; Copyright (C) 2010  Alexander Kahl <e-user@fsfe.org>
;;;; This file is part of cl-m4.
;;;; cl-m4 is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; cl-m4 is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :cl-m4)

(defclass m4-input-stream (lexer-input-stream)
  ((macro-stack :accessor m4-macro-stack
                :initform nil)))

(defgeneric m4-push-macro (m4-input-stream macro)
  (:method ((stream m4-input-stream) macro)
    (push macro (m4-macro-stack stream))))

(defgeneric m4-pop-macro (m4-input-stream)
  (:method ((stream m4-input-stream))
    (pop (m4-macro-stack stream))))

(defmethod stream-read-token :around ((stream m4-input-stream) &optional (peek nil))
  (declare (ignore peek))
  (if (m4-macro-stack stream)
      (values :macro-token (m4-pop-macro stream))
    (call-next-method)))
