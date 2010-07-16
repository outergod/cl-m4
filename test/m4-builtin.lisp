;;;; cl-m4 - m4-builtin.lisp
;;;; Copyright (C) 2010  Alexander Kahl <e-user@fsfe.org>
;;;; This file is part of cl-m4.
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

(in-package :cl-m4)

(shadowing-import
 '(macro-dnl-invocation-condition macro-defn-invocation-condition)
 (find-package :cl-m4-test))

(in-package :cl-m4-test)

(in-suite m4)

(defm4test macro-dnl-with-args "dnl" ("foo")
  :signal macro-dnl-invocation-condition
  :error (format nil "cl-m4:?:?: excess arguments to builtin `dnl' ignored~%"))

(defm4test macro-dnl-with-empty-args "dnl" (nil)
  :signal macro-dnl-invocation-condition
  :error (format nil "cl-m4:?:?: excess arguments to builtin `dnl' ignored~%"))

(defm4test macro-defn-no-args "defn" ())

(defm4test macro-defn-empty-args "defn" ("defn")
  :signal macro-defn-invocation-condition)
