;;;; evol - m4-composite.lisp
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

; TODO format
;; (deftest composite-array ()
;;   (m4-test
;; #>m4>

;; m4

;; #>m4>

;; m4

;;:depends (list "define" "defn" "format")))


(deftest composite-exch ()
  (m4-test
#>m4>
define(`exch', `$2, $1')
exch(`arg1', `arg2')
define(exch(``expansion text'', ``macro''))
macro
m4

#>m4>

arg2, arg1

expansion text
m4

:depends (list "define")))


(deftest composite-nargs ()
  (m4-test
#>m4>
define(`nargs', `$#')
nargs
nargs()
nargs(`arg1', `arg2', `arg3')
nargs(`commas can be quoted, like this')
nargs(arg1#inside comments, commas do not separate arguments
                       still arg1)
nargs((unquoted parentheses, like this, group arguments))
nargs(`('quoted parentheses, like this, don't group arguments`)')
m4

#>m4>

0
1
3
1
1
1
3
m4

:depends (list "define")))


(deftest composite-nargs-underquoted ()
  (m4-test
#>m4>
define(underquoted, $#)
oops)
underquoted
m4

#>m4>

0)
oops
m4

:depends (list "define")))


(deftest composite-echo ()
  (m4-test
#>m4>
define(`echo', `$*')
define(`arg1', `correct')
echo(`arg1',    arg2, arg3 , arg4)
define(`echo', `$@')
define(`arg1', `error')
echo(`arg1',    arg2, arg3 , arg4)
m4

#>m4>


correct,arg2,arg3 ,arg4


arg1,arg2,arg3 ,arg4
m4

:depends (list "define")))

(deftest composite-reverse ()
  (m4-test
#>m4>
define(`reverse', `ifelse(`$#', `0', , `$#', `1', ``$1'',
                          `reverse(shift($@)), `$1'')')
reverse
reverse(`foo')
reverse(`foo', `bar', `gnats', `and gnus')
m4

#>m4>


foo
and gnus, gnats, bar, foo
m4

:depends (list "define" "ifelse" "shift")))


; TODO incr
;; (deftest composite-cond ()
;;   (m4-test
;; #>m4>
;; define(`cond',
;; `ifelse(`$#', `1', `$1',
;;         `ifelse($1, `$2', `$3',
;;                 `$0(shift(shift(shift($@))))')')')dnl
;; define(`side', `define(`counter', incr(counter))$1')dnl
;; define(`example1',
;; `define(`counter', `0')dnl
;; ifelse(side(`$1'), `yes', `one comparison: ',
;;        side(`$1'), `no', `two comparisons: ',
;;        side(`$1'), `maybe', `three comparisons: ',
;;        `side(`default answer: ')')counter')dnl
;; define(`example2',
;; `define(`counter', `0')dnl
;; cond(`side(`$1')', `yes', `one comparison: ',
;;      `side(`$1')', `no', `two comparisons: ',
;;      `side(`$1')', `maybe', `three comparisons: ',
;;      `side(`default answer: ')')counter')dnl
;; example1(`yes')
;; example1(`no')
;; example1(`maybe')
;; example1(`feeling rather indecisive today')
;; example2(`yes')
;; example2(`no')
;; example2(`maybe')
;; example2(`feeling rather indecisive today')
;; m4

;; #>m4>
;; one comparison: 3
;; two comparisons: 3
;; three comparisons: 3
;; default answer: 4
;; one comparison: 1
;; two comparisons: 2
;; three comparisons: 3
;; default answer: 4
;; m4
;;
;;:depends (list "define" "ifelse" "shift" "incr")))


(deftest composite-cleardivert ()
  (m4-test
#>m4>
define(`cleardivert',
`pushdef(`_n', divnum)divert(`-1')undivert($@)divert(_n)popdef(`_n')')dnl
divert(`1')
Diversion one: divnum
divert(`2')
Diversion two: divnum
divert(`3')dnl
Diversion three: divnum
cleardivert(1,2)dnl
m4

#>m4>
Diversion three: 3
m4

:depends (list "define" "pushdef" "divnum" "divert" "undivert" "popdef")))


(deftest composite-cleardivert-fixed ()
  (m4-test
#>m4>
define(`cleardivert',
  `pushdef(`_num', divnum)divert(`-1')ifelse(`$#', `0',
    `undivert`'', `undivert($@)')divert(_num)popdef(`_num')')dnl
divert(`1')
Diversion one: divnum
divert(`2')
Diversion two: divnum
divert(`3')
Diversion three: divnum
cleardivert`'dnl
m4

#>m4>
m4

:depends (list "define" "pushdef" "divnum" "divert" "ifelse" "undivert" "popdef")))
