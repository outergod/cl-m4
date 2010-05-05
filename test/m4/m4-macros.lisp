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


; depends: define, defn, format
;; (deftest composite-array ()
;;   (m4-test
;; #>m4>

;; m4

;; #>m4>

;; m4))


; depends: define, $\d
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
m4))


; depends: define, $#
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
m4))


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
m4))


; depends: define, $\*, $\@
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
m4))


; "For example, if foo is a macro,
;     foo(() (`(') `(')
; is a macro call, with one argument, whose value is ‘() (() (’"
; depends: define, $\d
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


; depends: define, $\d
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


; "Undefining a macro inside that macro's expansion is safe; the macro still
; expands to the definition that was in effect at the ‘(’"
; depends: define, undefine, $\d
; depends: define, undefine, $\d 
(deftest gnu-m4-5.4 ()
  (m4-test
#>m4>
define(`f', ``$0':$1')
f(f(f(undefine(`f')`hello world')))
f(`bye')m4

#>m4>

f:f:f:hello world
f(bye)m4))


; depends: define, pushdef, popdef
(deftest gnu-m4-5.6-1 ()
  (m4-test
#>m4>
define(`foo', `Expansion one.')
foo
pushdef(`foo', `Expansion two.')
foo
pushdef(`foo', `Expansion three.')
pushdef(`foo', `Expansion four.')
popdef(`foo')
foo
popdef(`foo', `foo')
foo
popdef(`foo')
foo
m4

#>m4>

Expansion one.

Expansion two.



Expansion three.

Expansion one.

foo
m4))


; depends: define, undefine, pushdef
(deftest gnu-m4-5.6-2 ()
  (m4-test
#>m4>
define(`foo', `Expansion one.')
foo
pushdef(`foo', `Expansion two.')
foo
define(`foo', `Second expansion two.')
foo
undefine(`foo')
foo
m4

#>m4>

Expansion one.

Expansion two.

Second expansion two.

foo
m4))


; depends: define, indir
(deftest gnu-m4-5.7-1 ()
  (m4-test
#>m4>
define(`$$internal$macro', `Internal macro (name `$0')')
$$internal$macro
indir(`$$internal$macro')
m4

#>m4>

$$internal$macro
Internal macro (name $$internal$macro)
m4))


; depends: define, undefine, indir
(deftest gnu-m4-5.7-2 ()
  (m4-test
#>m4>
define(`f', `1')
f(define(`f', `2'))
indir(`f', define(`f', `3'))
indir(`f', undefine(`f'))
m4

#>m4>

1
3

m4

#>m4>WARNING: undefined macro `f'

m4))


;; ; depends: indir, defn, divnum, define
;; (deftest gnu-m4-5.7-3 ()
;;   (m4-test
;; #>m4>
;; indir(defn(`defn'), `divnum')
;; indir(`define', defn(`defn'), `divnum')
;; indir(`define', `foo', defn(`divnum'))
;; foo
;; indir(`divert', defn(`foo'))
;; m4

;; #>m4>



;; 0

;; m4

;; #>m4>
;; error-->m4:stdin:1: Warning: indir: invalid macro name ignored
;; error-->m4:stdin:2: Warning: define: invalid macro name ignored
;; error-->m4:stdin:5: empty string treated as 0 in builtin `divert'
;; m4))


; TODO divnum
; depends: pushdef, define, undefine, builtin, defn, divnum
(deftest gnu-m4-5.8-1 ()
  (m4-test
#>m4>
pushdef(`define', `hidden')
undefine(`undefine')
define(`foo', `bar')
foo
dnl builtin(`define', `foo', defn(`divnum'))
dnl foo
builtin(`define', `foo', `BAR')
foo
undefine(`foo')
foo
builtin(`undefine', `foo')
foo
m4

#>m4>


hidden
foo

BAR
undefine(foo)
BAR

foo
m4))


; depends: builtin, indir
(deftest gnu-m4-5.8-3 ()
  (m4-test
#>m4>
builtin
builtin()
builtin(`builtin')
builtin(`builtin',)
builtin(`builtin', ``'
             ')
indir(`index')
m4

#>m4>
builtin





m4

#>m4>undefined builtin `'
Warning: too few arguments to builtin `builtin'
undefined builtin `'
undefined builtin ``'
'
Warning: too few arguments to builtin `index'
m4))
