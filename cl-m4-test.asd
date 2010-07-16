;;;; cl-m4 - cl-m4-test.asd
;;;; Copyright (C) 2009 2010  Alexander Kahl <e-user@fsfe.org>
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

(in-package :cl-user)

(defpackage :cl-m4-test-system
  (:use :cl :asdf))

(in-package :cl-m4-test-system)

(asdf:defsystem :cl-m4-test
                :description "cl-m4 test package"
                :version "0.0.1"
                :author "Alexander Kahl <e-user@fsfe.org>"
                :license "GPLv3+"
                :depends-on (:cl-m4 :stefil)
                :components
                ((:module "test"
                          :components
                          ((:file "package")
                           (:module "cffi-regex"
                            :depends-on ("package")
                            :components
                            ((:file "regex")))
                           (:file "m4")
                           (:file "m4-builtin" :depends-on ("package" "m4"))
                           (:file "m4-parser"  :depends-on ("package" "m4"))
                           (:file "m4-macros"  :depends-on ("package" "m4"))
                           (:file "m4-composite"  :depends-on ("package" "m4"))))))
