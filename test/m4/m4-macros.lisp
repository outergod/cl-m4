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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *cwd* (make-pathname
                       :directory (pathname-directory (or *compile-file-truename* "")))))

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

:error #>m4eof>cl-m4:1:92: undefined macro `f'
m4eof))


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

;; :error #>m4>
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

:error #>m4eof>cl-m4:1:18: undefined builtin `'
cl-m4:1:37: too few arguments to builtin `builtin'
cl-m4:1:57: undefined builtin `'
cl-m4:2:15: undefined builtin ``'
             '
m4eof))


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

:error #>m4eof>cl-m4:1:186: excess arguments to builtin `ifdef' ignored
m4eof))


; depends: ifelse
(deftest gnu-m4-6.2-1 ()
  (m4-test
#>m4>
ifelse(`some comments')
ifelse(`foo', `bar')
m4

#>m4>


m4

:error #>m4eof>cl-m4:1:45: too few arguments to builtin `ifelse'
m4eof))


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

:error #>m4eof>cl-m4:1:46: excess arguments to builtin `ifelse' ignored
cl-m4:1:224: excess arguments to builtin `ifelse' ignored
m4eof))


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

:error #>m4>foo:	Hello world.
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

:error #>m4eof>f:	`$0'1
cl-m4:1:96: undefined macro `f'

m4eof))


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

:error #>m4eof>cl-m4:2:27: excess arguments to builtin `dnl' ignored
m4eof))


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


; depends: changequote, dnl, define, $\*, $\d
(deftest gnu-m4-8.2-6 ()
  (m4-test
#>m4>
changequote(`[', `]')dnl
define([a], [1, (b)])dnl
define([b], [2])dnl
define([quote], [[$*]])dnl
define([expand], [_$0(($1))])dnl
define([_expand],
  [changequote([(], [)])$1changequote`'changequote(`[', `]')])dnl
expand([a, a, [a, a], [[a, a]]])
quote(a, a, [a, a], [[a, a]])
m4

#>m4>
1, (2), 1, (2), a, a, [a, a]
1,(2),1,(2),a, a,[a, a]
m4))


; depends: define, changequote
(deftest gnu-m4-8.2-7 ()
  (m4-test
#>m4>
define(`hi', `HI')
changequote(`""', `"')
""hi"""hi"
""hi" ""hi"
""hi"" "hi"
changequote
`hi`hi'hi'
changequote(`"', `"')
"hi"hi"hi"
m4

#>m4>


hihi
hi hi
hi" "HI"

hi`hi'hi

hiHIhi
m4))


; depends: define, changecom
(deftest gnu-m4-8.3-1 ()
  (m4-test
#>m4>
define(`comment', `COMMENT')
# A normal comment
changecom(`/*', `*/')
# Not a comment anymore
But: /* this is a comment now */ while this is not a comment
m4

#>m4>

# A normal comment

# Not a COMMENT anymore
But: /* this is a comment now */ while this is not a COMMENT
m4))


; depends: define, changecom
(deftest gnu-m4-8.3-2 ()
  (m4-test
#>m4>
define(`comment', `COMMENT')
changecom
# Not a comment anymore
changecom(`#', `')
# comment again
m4

#>m4>


# Not a COMMENT anymore

# comment again
m4))


; depends: define, changecom
(deftest gnu-m4-8.3-3 ()
  (m4-test
#>m4>
define(`hi', `HI')
define(`hi1hi2', `hello')
changecom(`q', `Q')
q hi Q hi
changecom(`1', `2')
hi1hi2
hi 1hi2
m4

#>m4>



q hi Q HI

hello
HI 1hi2
m4))


; depends: define, changecom
(deftest gnu-m4-8.3-4 ()
  (m4-test
#>m4>
define(`echo', `$#:$*:$@:')
define(`hi', `HI')
changecom(`(',`)')
echo(hi)
changecom
changecom(`((', `))')
echo(hi)
echo((hi))
changecom(`,', `)')
echo(hi,hi)bye)
changecom
echo(hi,`,`'hi',hi)
echo(hi,`,`'hi',hi`'changecom(`,,', `hi'))
m4

#>m4>



0:::(hi)


1:HI:HI:
0:::((hi))

1:HI,hi)bye:HI,hi)bye:

3:HI,,HI,HI:HI,,`'hi,HI:
3:HI,,`'hi,HI:HI,,`'hi,HI:
m4))


; depends: define, m4wrap
(deftest gnu-m4-8.5-1 ()
  (m4-test
#>m4eof>
define(`cleanup', `This is the `cleanup' action.
')
m4wrap(`cleanup')
This is the first and last normal input line.
m4eof

#>m4>


This is the first and last normal input line.
This is the cleanup action.
m4))


;; ; TODO eval, decr
;; ; depends: define, ifelse, $\d, eval, m4wrap, decr
;; (deftest gnu-m4-8.5-2 ()
;;   (m4-test
;; #>m4eof>
;; define(`f', `ifelse(`$1', `0', `Answer: 0!=1
;; ', eval(`$1>1'), `0', `Answer: $2$1=eval(`$2$1')
;; ', `m4wrap(`f(decr(`$1'), `$2$1*')')')')
;; f(`10')
;; m4eof

;; #>m4>


;; Answer: 10*9*8*7*6*5*4*3*2*1=3628800
;; m4))


; depends: define, m4wrap
(deftest gnu-m4-8.5-3 ()
  (m4-test
#>m4eof>
define(`aa', `AA
')
m4wrap(`a')m4wrap(`a')
m4eof

#>m4>


AA
m4))


; depends: include, sinclude
(deftest gnu-m4-9.1-1 ()
  (m4-test
#>m4>
include(`none')
include()
sinclude(`none')
sinclude()
m4
#>m4>




m4

:error #>m4eof>cl-m4:1:16: cannot open `none': No such file or directory
cl-m4:1:26: cannot open `': No such file or directory
m4eof))


; depends: define, include
(deftest gnu-m4-9.1-2 ()
  (m4-test
#>m4eof>
define(`foo', `FOO')
include(`incl.m4')
m4eof

#>m4>

Include file start
FOO
Include file end

m4
:include-path (list (merge-pathnames "fixtures/" *cwd*))))


; depends: define, include
(deftest gnu-m4-9.1-3 ()
  (m4-test
#>m4eof>
define(`bar', include(`incl.m4'))
This is `bar':  >>bar<<
m4eof

#>m4>

This is bar:  >>Include file start
foo
Include file end
<<
m4
:include-path (list (merge-pathnames "fixtures/" *cwd*))))


; depends: divert
(deftest gnu-m4-10.1-1 ()
  (m4-test
#>m4>
divert(`1')
This text is diverted.
divert
This text is not diverted.
m4

#>m4>

This text is not diverted.

This text is diverted.
m4))


; depends: define, divert, m4wrap
(deftest gnu-m4-10.1-2 ()
  (m4-test
#>m4eof>
define(`text', `TEXT')
divert(`1')`diverted text.'
divert
m4wrap(`Wrapped text precedes ')
m4eof

#>m4>



Wrapped TEXT precedes diverted text.
m4))


; depends: define, divert
(deftest gnu-m4-10.1-3 ()
  (m4-test
#>m4>
divert(`-1')
define(`foo', `Macro `foo'.')
define(`bar', `Macro `bar'.')
divert
m4

#>m4>

m4))


;; ; TODO eval
;; ; depends: divert, eval
;; (deftest gnu-m4-10.1-4 ()
;;   (m4-test
;; #>m4>
;; divert(eval(`1<<28'))world
;; divert(`2')hello
;; m4

;; #>m4>
;; hello
;; world
;; m4))


; depends: define, divert, ifelse, builtin, $#, $@, $\d
(deftest gnu-m4-10.1-5 ()
  (m4-test
#>m4>
We decided to divert the stream for irrigation.
define(`divert', `ifelse(`$#', `0', ``$0'', `builtin(`$0', $@)')')
divert(`-1')
Ignored text.
divert(`0')
We decided to divert the stream for irrigation.
m4

#>m4>
We decided to  the stream for irrigation.


We decided to divert the stream for irrigation.
m4))


; depends: divert, undivert
(deftest gnu-m4-10.2-1 ()
  (m4-test
#>m4>
divert(`1')
This text is diverted.
divert
This text is not diverted.
undivert(`1')
m4

#>m4>

This text is not diverted.

This text is diverted.

m4))


; depends: divert, undivert
(deftest gnu-m4-10.2-2 ()
  (m4-test
#>m4>
divert(`1')diverted text
divert
undivert()
undivert(`0')
undivert
divert(`1')more
divert(`2')undivert(`1')diverted text`'divert
undivert(`1')
undivert(`2')
m4

#>m4>



diverted text



more
diverted text
m4))


; depends: divert, undivert
(deftest gnu-m4-10.2-3 ()
  (m4-test
#>m4>
divert(`1')
This text is diverted first.
divert(`0')undivert(`1')dnl
undivert(`1')
divert(`1')
This text is also diverted but not appended.
divert(`0')undivert(`1')dnl
m4

#>m4>

This text is diverted first.


This text is also diverted but not appended.
m4))


; depends: divert, undivert, dnl
(deftest gnu-m4-10.2-4 ()
  (m4-test
#>m4>
divert(`1')one
divert(`2')two
divert(`3')three
divert(`2')undivert`'dnl
divert`'undivert`'dnl
m4

#>m4>
two
one
three
m4))


; depends: define, undivert, include
(deftest gnu-m4-10.2-5 ()
  (m4-test
#>m4>
define(`bar', `BAR')
undivert(`foo')
include(`foo')
m4

#>m4>

bar

BAR

m4

:include-path (list (merge-pathnames "fixtures/" *cwd*))))


; depends: divert, undivert, dnl
(deftest gnu-m4-10.2-6 ()
  (m4-test
#>m4>
divert(`1')diversion one
divert(`2')undivert(`foo')dnl
divert(`3')diversion three
divert`'dnl
undivert(`1', `2', `foo', `3')dnl
m4

#>m4>
diversion one
bar
bar
diversion three
m4

:include-path (list (merge-pathnames "fixtures/" *cwd*))))


; depends: divert, divnum
(deftest gnu-m4-10.3 ()
  (m4-test
#>m4>
Initial divnum
divert(`1')
Diversion one: divnum
divert(`2')
Diversion two: divnum
m4

#>m4>
Initial 0

Diversion one: 1

Diversion two: 2
m4))


; depends: divert, undivert
(deftest gnu-m4-10.4 ()
  (m4-test
#>m4>
divert(`1')
Diversion one: divnum
divert(`2')
Diversion two: divnum
divert(`-1')
undivert
m4

#>m4>
m4))


; depends: define, pushdef, divnum, divert, undivert, popdef, $@
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
m4))


; depends: define, pushdef, divnum, divert, ifelse, undivert, popdef, $@
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
m4))


; depends: len
(deftest gnu-m4-11.1 ()
  (m4-test
#>m4>
len()
len(`abcdef')
m4

#>m4>
0
6
m4))


; depends: index
(deftest gnu-m4-11.2-1 ()
  (m4-test
#>m4>
index(`gnus, gnats, and armadillos', `nat')
index(`gnus, gnats, and armadillos', `dag')
m4

#>m4>
7
-1
m4))


; depends: index
(deftest gnu-m4-11.2-2 ()
  (m4-test
#>m4>
index(`abc')
index(`abc', `')
index(`abc', `b')
m4

#>m4>
0
0
1
m4

:error #>m4eof>cl-m4:1:13: too few arguments to builtin `index'
m4eof))


; depends: substr
(deftest gnu-m4-11.4-1 ()
  (m4-test
#>m4>
substr(`gnus, gnats, and armadillos', `6')
substr(`gnus, gnats, and armadillos', `6', `5')
m4

#>m4>
gnats, and armadillos
gnats
m4))


; depends: substr
(deftest gnu-m4-11.4-2 ()
  (m4-test
#>m4>
substr(`abc')
substr(`abc',)
m4

#>m4>
abc
abc
m4

:error #>m4eof>cl-m4:1:14: too few arguments to builtin `substr'
cl-m4:1:29: empty string treated as 0 in builtin `substr'
m4eof))


; depends: translit
(deftest gnu-m4-11.5-1 ()
  (m4-test
#>m4>
translit(`GNUs not Unix', `A-Z')
translit(`GNUs not Unix', `a-z', `A-Z')
translit(`GNUs not Unix', `A-Z', `z-a')
translit(`+,-12345', `+--1-5', `<;>a-c-a')
translit(`abcdef', `aabdef', `bcged')
m4

#>m4>
s not nix
GNUS NOT UNIX
tmfs not fnix
<;>abcba
bgced
m4))


; depends: translit
(deftest gnu-m4-11.5-2 ()
  (m4-test
#>m4>
translit(`abc')
m4

#>m4>
abc
m4

:error #>m4eof>cl-m4:?:?: too few arguments to builtin `translit'
m4eof))
