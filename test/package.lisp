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

(in-package :cl-m4-test-system)

(defpackage :cl-m4-test
  (:use :cl :cffi-regex :hu.dwim.stefil)
  (:export :all)
  (:shadowing-import-from :cl-m4 m4-macro with-m4-lib *m4-lib* *m4-parse-row* *m4-parse-column* *m4-runtime-lib*))

(in-package :cl-m4-test)

(in-suite root-suite)
(defsuite all)
