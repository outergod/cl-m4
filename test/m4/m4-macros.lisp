;;;; evol - m4-macros.lisp
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

(in-suite m4)

; "For example, if foo is a macro,
;     foo(() (`(') `(')
; is a macro call, with one argument, whose value is ‘() (() (’"
(deftest gnu-m4-4.4-1 ()
  (m4-test
#>m4>
define(`foo', `[$1] [$2]')
foo(() (`(') `(')
foo(`() (() (')
m4

#>m4>

[() (() (] []
[() (() (] []
m4))


(deftest gnu-m4-4.4-2 ()
  (m4-test
#>m4>
define(`active', `ACT, IVE')
define(`show', `$1 $1')
show(active)
show(`active')
show(``active'')
m4

#>m4>


ACT ACT
ACT, IVE ACT, IVE
active active
m4))


; "Undefining a macro inside that macro's expansion is safe; the macro still expands to the definition that was in effect at the ‘(’"
(deftest gnu-m4-5.4 ()
  (m4-test
#>m4>
define(`f', ``$0':$1')
f(f(f(undefine(`f')`hello world')))
f(`bye')m4

#>m4>

f:f:f:hello world
f(bye)m4))
