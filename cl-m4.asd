;;;; cl-m4 - cl-m4.asd
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

(in-package :cl-user)

(defpackage :cl-m4-system
  (:use :cl :asdf))

(in-package :cl-m4-system)

;;; CFFI-Grovel is needed for processing grovel-file components
(cl:eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op 'cffi-grovel))

(asdf:defsystem :cl-m4
                :description "Common Lisp re-implementation of GNU M4"
                :version "0.0.1"
                :author "Alexander Kahl <e-user@fsfe.org>"
                :license "GPLv3+"
                :depends-on (:external-program :cl-ppcre :alexandria
                             :trivial-gray-streams :cffi)
                :components
                ((:module "src"
                          :components
                          ((:file "package")
                           (:file "ring-buffer" :depends-on ("package"))
                           (:file "heredoc"     :depends-on ("package" "ring-buffer"))
                           (:module "cffi-regex"
                            :components
                            ((cffi-grovel:grovel-file "cffi-regex-grovel")
                             (:file "cffi-regex"  :depends-on ("cffi-regex-grovel"))
                             (:file "regex"       :depends-on ("cffi-regex")))
                            :depends-on ("package"))
                           (:file "m4-util" :depends-on ("package"))
                           (:file "m4-lexer" :depends-on ("package"))
                           (:file "m4-builtin" :depends-on ("package" "m4-util"))
                           (:file "m4-parser" :depends-on ("package" "m4-util" "m4-lexer"))))))
