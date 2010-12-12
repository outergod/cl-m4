;;;; cl-m4 - package.lisp
;;;; Copyright (C) 2009, 2010  Alexander Kahl <e-user@fsfe.org>
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

(in-package :cl-m4-system)

(defpackage :cffi-regex
  (:use :cl :cffi)
  (:export :regex-search :regex-match :regex-compilation-failure :regex-internal-error))

(defpackage :cl-m4
  (:use :cl :external-program :cffi-regex :graylex)
  (:shadow :copy-stream :copy-file)
  (:export :process-m4))
