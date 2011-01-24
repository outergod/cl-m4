;;;; cl-m4 - m4-composite.lisp
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

(in-package :cl-m4-test)

(in-suite m4)

;;; 5 How to define new macros
;; 5.1 Defining a macro
; TODO format
;; (deftest composite-array ()
;;   (m4-test
;; #>m4>
;;
;; m4
;;
;; #>m4>
;;
;; m4
;;
;;:depends (list "define" "defn" "format")))


;; 5.2 Arguments to macros
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


;; 5.3 Special arguments to macros
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


;; 6.3 Recursion in m4
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


(deftest composite-join ()
  (m4-test
#>m4eof>
include(`join.m4')
join,join(`-'),join(`-', `'),join(`-', `', `')
joinall,joinall(`-'),joinall(`-', `'),joinall(`-', `', `')
join(`-', `1')
join(`-', `1', `2', `3')
join(`', `1', `2', `3')
join(`-', `', `1', `', `', `2', `')
joinall(`-', `', `1', `', `', `2', `')
join(`,', `1', `2', `3')
define(`nargs', `$#')dnl
nargs(join(`,', `1', `2', `3'))
m4eof

#>m4>

,,,
,,,-
1
1-2-3
123
1-2
-1---2-
1,2,3
1
m4

:include-path (list (relative-pathname "fixtures/gnu-m4-examples/"))
:depends (list "include" "define" "dnl" "ifelse" "shift" "divert")))


(deftest composite-quote ()
  (m4-test
#>m4eof>
include(`quote.m4')
-quote-dquote-dquote_elt-
-quote()-dquote()-dquote_elt()-
-quote(`1')-dquote(`1')-dquote_elt(`1')-
-quote(`1', `2')-dquote(`1', `2')-dquote_elt(`1', `2')-
define(`n', `$#')dnl
-n(quote(`1', `2'))-n(dquote(`1', `2'))-n(dquote_elt(`1', `2'))-
dquote(dquote_elt(`1', `2'))
dquote_elt(dquote(`1', `2'))
m4eof

#>m4>

----
--`'-`'-
-1-`1'-`1'-
-1,2-`1',`2'-`1',`2'-
-1-1-2-
``1'',``2''
``1',`2''
m4

:include-path (list (relative-pathname "fixtures/gnu-m4-examples/"))
:depends (list "include" "dnl" "define" "divert" "ifelse" "shift")))


;; TODO decr
;; (deftest composite-argn ()
;;   (m4-test
;; #>m4>
;; define(`argn', `ifelse(`$1', 1, ``$2'',
;;   `argn(decr(`$1'), shift(shift($@)))')')
;; argn(`1', `a')
;; define(`foo', `argn(`11', $@)')
;; foo(`a', `b', `c', `d', `e', `f', `g', `h', `i', `j', `k', `l')
;; m4

;; #>m4>

;; a

;; k
;; m4

;; :depends (list "define" "ifelse" "decr" "shift")))


;; TODO incr
;; — Composite: forloop (iterator, start, end, text)


;; — Composite: foreach (iterator, paren-list, text)
;; — Composite: foreachq (iterator, quote-list, text)
(deftest composite-foreach-1 ()
  (m4-test
#>m4eof>
include(`foreach.m4')
foreach(`x', (foo, bar, foobar), `Word was: x
')dnl
include(`foreachq.m4')
foreachq(`x', `foo, bar, foobar', `Word was: x
')dnl
m4eof

#>m4>

Word was: foo
Word was: bar
Word was: foobar

Word was: foo
Word was: bar
Word was: foobar
m4

:include-path (list (relative-pathname "fixtures/gnu-m4-examples/"))
:depends (list "include" "divert" "define" "pushdef" "popdef" "ifelse" "shift" "dnl")))


(deftest composite-foreach-2 ()
  (m4-test
#>m4eof>
include(`foreach.m4')
define(`_case', `  $1)
    $2=" $1";;
')dnl
define(`_cat', `$1$2')dnl
case $`'1 in
foreach(`x', `(`(`a', `vara')', `(`b', `varb')', `(`c', `varc')')',
        `_cat(`_case', x)')dnl
esac
m4eof

#>m4>

case $1 in
  a)
    vara=" a";;
  b)
    varb=" b";;
  c)
    varc=" c";;
esac
m4

:include-path (list (relative-pathname "fixtures/gnu-m4-examples/"))
:depends (list "include" "divert" "define" "pushdef" "popdef" "ifelse" "shift" "dnl")))



;; — Composite: stack_foreach (macro, action)
;; — Composite: stack_foreach_lifo (macro, action)
;; — Composite: define_blind (name, [value])
;; — Composite: curry (macro, ...)
;; — Composite: copy (source, dest)
;; — Composite: rename (source, dest)


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


;; 11.6 Substituting text by regular expression
(deftest composite-capitalize ()
  (m4-test
#>m4eof>
include(`capitalize.m4')
upcase(`GNUs not Unix')
downcase(`GNUs not Unix')
capitalize(`GNUs not Unix')
m4eof

#>m4>

GNUS NOT UNIX
gnus not unix
Gnus Not Unix
m4

:include-path (list (relative-pathname "fixtures/gnu-m4-examples/"))
:depends (list "divert" "define" "translit" "regexp" "patsubst" "dnl")))

(deftest composite-patreg ()
  (m4-test
#>m4>
define(`patreg',
`patsubst($@)
regexp($@)')dnl
patreg(`bar foo baz Foo', `foo\|Foo', `FOO')
patreg(`aba abb 121', `\(.\)\(.\)\1', `\2\1\2')
m4

#>m4>
bar FOO baz FOO
FOO
bab abb 212
bab
m4

:depends (list "define" "patsubst" "regexp" "dnl")))

