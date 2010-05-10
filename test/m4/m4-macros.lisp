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

; TODO format
; depends: define, defn, format
;; (deftest composite-array ()
;;   (m4-test
;; #>m4>

;; m4

;; #>m4>

;; m4))


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


; TODO divnum
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


; TODO index
; depends: builtin, indir, index
(deftest gnu-m4-5.8-3 ()
  (m4-test
#>m4>
builtin
builtin()
builtin(`builtin')
builtin(`builtin',)
builtin(`builtin', ``'
             ')
dnl indir(`index')
m4

#>m4>
builtin




m4

#>m4>WARNING: undefined builtin `'


WARNING: too few arguments to builtin `builtin'


WARNING: undefined builtin `'


WARNING: undefined builtin ``'
             '

m4))



; depends: ifdef, define
(deftest gnu-m4-6.1 ()
  (m4-test
#>m4>
ifdef(`foo', ``foo' is defined', ``foo' is not defined')
define(`foo', `')
ifdef(`foo', ``foo' is defined', ``foo' is not defined')
ifdef(`no_such_macro', `yes', `no', `extra argument')
m4

#>m4>
foo is not defined

foo is defined
no
m4

#>m4>WARNING: excess arguments to builtin `ifdef' ignored

m4))


; depends: ifelse
(deftest gnu-m4-6.2-1 ()
  (m4-test
#>m4>
ifelse(`some comments')
ifelse(`foo', `bar')
m4

#>m4>


m4

#>m4>WARNING: too few arguments to builtin `ifelse'

m4))


; depends: ifelse, define
(deftest gnu-m4-6.2-2 ()
  (m4-test
#>m4>
ifelse(`foo', `bar', `true')
ifelse(`foo', `foo', `true')
define(`foo', `bar')
ifelse(foo, `bar', `true', `false')
ifelse(foo, `foo', `true', `false')
m4
#>m4>

true

true
false
m4))



; depends: ifelse, define
(deftest gnu-m4-6.2-3 ()
  (m4-test
#>m4>
define(`foo', `ifelse(`$#', `0', ``$0'', `arguments:$#')')
foo
foo()
foo(`a', `b', `c')
m4
#>m4>

foo
arguments:1
arguments:3
m4))


; depends: ifelse, define
(deftest gnu-m4-6.2-4 ()
  (m4-test
#>m4>
ifelse(`foo', `bar', `third', `gnu', `gnats')
ifelse(`foo', `bar', `third', `gnu', `gnats', `sixth')
ifelse(`foo', `bar', `third', `gnu', `gnats', `sixth', `seventh')
ifelse(`foo', `bar', `3', `gnu', `gnats', `6', `7', `8')
m4

#>m4>
gnu

seventh
7
m4

#>m4>WARNING: excess arguments to builtin `ifelse' ignored


WARNING: excess arguments to builtin `ifelse' ignored

m4))


; depends: shift
(deftest gnu-m4-6.3-1 ()
  (m4-test
#>m4>
shift
shift(`bar')
shift(`foo', `bar', `baz')
m4

#>m4>
shift

bar,baz
m4))


; depends: define, ifelse, $\d, $#, shift
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
m4))

; TODO incr
; depends: define, ifelse, shift, incr, $\d
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
;; m4))


; depends: define, dumpdef
(deftest gnu-m4-7.1-1 ()
  (m4-test
#>m4>
define(`foo', `Hello world.')
dumpdef(`foo')
dumpdef(`define')
m4

#>m4>



m4

#>m4>foo:	Hello world.
define:	<define>
m4))


; depends: pushdef, popdef, dumpdef
(deftest gnu-m4-7.1-2 ()
  (m4-test
#>m4>
pushdef(`f', ``$0'1')pushdef(`f', ``$0'2')
f(popdef(`f')dumpdef(`f'))
f(popdef(`f')dumpdef(`f'))
m4

#>m4>

f2
f1
m4

#>m4>f:	`$0'1

WARNING: undefined macro `f'

m4))


;; TODO traceon, traceoff, debugmode, debugfile


; depends: define, dnl
(deftest gnu-m4-8.1-1 ()
  (m4-test
#>m4>
define(`foo', `Macro `foo'.')dnl A very simple macro, indeed.
foo
m4

#>m4>
Macro foo.
m4))


; depends: define, dnl
(deftest gnu-m4-8.1-2 ()
  (m4-test
#>m4>
dnl(`args are ignored, but side effects occur',
define(`foo', `like this')) while this text is ignored: undefine(`foo')
See how `foo' was defined, foo?
m4

#>m4>
See how foo was defined, like this?
m4

#>m4>WARNING: excess arguments to builtin `dnl' ignored

m4))


;; ; TODO m4wrap
;; ; depends: m4wrap, define, dnl
;; (deftest gnu-m4-8.1-3 ()
;;   (m4-test
;; #>m4>
;; m4wrap(`m4wrap(`2 hi
;; ')0 hi dnl 1 hi')
;; define(`hi', `HI')m4

;; #>m4>
;; 0 HI 2 HI
;; m4

;; #>m4>WARNING: end of file treated as newline

;; m4))


; depends: define, changequote
(deftest gnu-m4-8.2-1 ()
  (m4-test
#>m4>
changequote(`[', `]')
define([foo], [Macro [foo].])
foo
m4

#>m4>


Macro foo.
m4))


; depends: define, changequote
(deftest gnu-m4-8.2-2 ()
  (m4-test
#>m4>
changequote(`[[[', `]]]')
define([[[foo]]], [[[Macro [[[[[foo]]]]].]]])
foo
m4

#>m4>


Macro [[foo]].
m4))


; depends: define, changequote
(deftest gnu-m4-8.2-3 ()
  (m4-test
#>m4>
define(`foo', `Macro `FOO'.')
changequote(`', `')
foo
`foo'
changequote(`,)
foo
m4

#>m4>


Macro `FOO'.
`Macro `FOO'.'

Macro FOO.
m4))


; depends: define, changequote
(deftest gnu-m4-8.2-4 ()
  (m4-test
#>m4>
define(`echo', `$@')
define(`hi', `HI')
changequote(`q', `Q')
q hi Q hi
echo(hi)
changequote
changequote(`-', `EOF')
- hi EOF hi
changequote
changequote(`1', `2')
hi1hi2
hi 1hi2
m4

#>m4>



q HI Q HI
qHIQ


 hi  HI


hi1hi2
HI hi
m4))


; depends: define, changequote, $#, $@
(deftest gnu-m4-8.2-5 ()
  (m4-test
#>m4>
define(`echo', `$#:$@:')
define(`hi', `HI')
changequote(`(',`)')
echo(hi)
changequote
changequote(`((', `))')
echo(hi)
echo((hi))
changequote
changequote(`,', `)')
echo(hi,hi)bye)
m4

#>m4>



0::hi


1:HI:
0::hi


1:HIhibye:
m4))
