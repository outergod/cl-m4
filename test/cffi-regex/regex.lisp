;;;; evol - regex.lisp
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

(in-package :evol-test)

(in-suite all)
(defsuite cffi-regex)
(in-suite cffi-regex)

(defmacro with-regex-condition (startpos registers signal &body body)
  `(if signal
       (signals ,signal ,@body)
     (multiple-value-bind (startpos-result registers-result) ,@body
       (is (equal startpos-result ,startpos))
       (is (equal (coerce registers-result 'list) ,registers)))))

(deftest assert-regex-search (regex target-string &key (start 0) (end (length target-string))
                                                       (startpos start) registers signal)
  (with-regex-condition startpos registers signal
    (regex-search regex target-string :start start :end end)))

(deftest regex-gnu-m4-11.3-1 ()
  (assert-regex-search "\\<[a-z]\\w+" "GNUs not Unix" :startpos 5 :registers (list (list 5 8))))

(deftest regex-gnu-m4-11.3-2 ()
  (assert-regex-search "\<Q\w*" "GNUs not Unix" :startpos nil))

(deftest regex-gnu-m4-11.3-3 ()
  (assert-regex-search "\\(b\\)" "abc" :startpos 1 :registers (list (list 1 2) (list 1 2))))

(deftest regex-gnu-m4-11.3-4 ()
  (assert-regex-search "b" "abc" :startpos 1 :registers (list (list 1 2))))

(deftest regex-gnu-m4-11.3-5 ()
  (assert-regex-search "\\(\\(d\\)?\\)\\(c\\)" "abc" :startpos 2 :registers (list (list 2 3) (list 2 2)
                                                                                  (list -1 -1) (list 2 3))))
