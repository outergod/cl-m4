;;;; cl-m4 - m4-macros.lisp
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

;;; 4 How to invoke macros
;; 4.4 On Quoting Arguments to macros
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
m4

:depends (list "define")))


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
m4

:depends (list "define")))


;;; 5 How to define new macros
;; 5.4 Deleting a macro
; "Undefining a macro inside that macro's expansion is safe; the macro still
; expands to the definition that was in effect at the ‘(’"
(deftest gnu-m4-5.4 ()
  (m4-test
#>m4>
define(`f', ``$0':$1')
f(f(f(undefine(`f')`hello world')))
f(`bye')m4

#>m4>

f:f:f:hello world
f(bye)m4

:depends (list "define" "undefine")))


;; 5.5 Renaming macros
(deftest gnu-m4-5.5-1 ()
  (m4-test
#>m4>
define(`zap', defn(`undefine'))
zap(`undefine')
undefine(`zap')
m4

#>m4>


undefine(zap)
m4

:depends (list "define" "defn" "undefine")))


(deftest gnu-m4-5.5-2 ()
  (m4-test
#>m4>
define(`foo', `This is `$0'')
define(`bar', defn(`foo'))
bar
m4

#>m4>


This is bar
m4

:depends (list "define" "defn")))


(deftest gnu-m4-5.5-3 ()
  (m4-test
#>m4>
define(`string', `The macro dnl is very useful
')
string
defn(`string')
m4

#>m4>

The macro 
The macro dnl is very useful

m4

:depends (list "define" "defn")))


(deftest gnu-m4-5.5-4 ()
  (m4-test
#>m4>
define(`foo', a'a)
define(`a', `A')
define(`echo', `$@')
foo
defn(`foo')
echo(foo)
m4

#>m4>



A'A
aA'
AA'
m4

:depends (list "define" "defn")))


(deftest gnu-m4-5.5-5 ()
  (m4-test
#>m4>
define(`l', `<[>')define(`r', `<]>')
changequote(`[', `]')
defn([l])defn([r])
])
defn([l], [r])
m4

#>m4>


<[>]defn([r])
)
<[>][<]>
m4

:depends (list "define" "changequote" "defn")))


(deftest gnu-m4-5.5-6 ()
  (m4-test
#>m4>
defn(`defn')
define(defn(`divnum'), `cannot redefine a builtin token')
divnum
len(defn(`divnum'))
m4

#>m4>


0
0
m4

:error #>m4eof>cl-m4:3:57: define: invalid macro name ignored
m4eof

:depends (list "defn" "define" "divnum" "len")))


; TODO traceon traceoff
;; (deftest gnu-m4-5.5-7 ()
;;   (m4-test
;; #>m4>
;; define(`a', `A')define(`AA', `b')
;; traceon(`defn', `define')
;; defn(`a', `divnum', `a')
;; define(`mydivnum', defn(`divnum', `divnum'))mydivnum
;; traceoff(`defn', `define')
;; m4

;; #>m4>


;; AA


;; m4

;; :error #>m4eof>cl-m4:stdin:3: cannot concatenate builtin `divnum'
;; cl-m4trace: -1- defn(`a', `divnum', `a') -> ``A'`A''
;; cl-m4:stdin:4: cannot concatenate builtin `divnum'
;; cl-m4:stdin:4: cannot concatenate builtin `divnum'
;; cl-m4trace: -2- defn(`divnum', `divnum')
;; cl-m4trace: -1- define(`mydivnum', `')
;; m4eof

;; :depends (list "define" "traceon" "defn" "divnum" "traceoff")))


;; 5.6 Temporarily redefining macros
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
m4

:depends (list "define" "pushdef" "popdef")))


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
m4

:depends (list "define" "undefine" "pushdef")))


;; 5.7 Indirect call of macros
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
m4

:depends (list "define" "indir")))


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

:error #>m4eof>cl-m4:5:25: undefined macro `f'
m4eof

:depends (list "define" "undefine" "indir")))


(deftest gnu-m4-5.7-3 ()
  (m4-test
#>m4>
indir(defn(`defn'), `divnum')
indir(`define', defn(`defn'), `divnum')
indir(`define', `foo', defn(`divnum'))
foo
indir(`divert', defn(`foo'))
m4

#>m4>



0

m4

:error #>m4eof>cl-m4:2:29: indir: invalid macro name ignored
cl-m4:3:39: define: invalid macro name ignored
cl-m4:6:28: empty string treated as 0 in builtin `divert'
m4eof

:depends (list "indir" "defn" "divnum" "define")))


;; 5.8 Indirect call of builtins
(deftest gnu-m4-5.8-1 ()
  (m4-test
#>m4>
pushdef(`define', `hidden')
undefine(`undefine')
define(`foo', `bar')
foo
builtin(`define', `foo', defn(`divnum'))
foo
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

0

BAR
undefine(foo)
BAR

foo
m4

:depends (list "pushdef" "define" "undefine" "builtin" "defn" "divnum")))


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

:error #>m4eof>cl-m4:3:9: undefined builtin `'
cl-m4:4:18: too few arguments to builtin `builtin'
cl-m4:5:19: undefined builtin `'
cl-m4:7:15: undefined builtin ``'
             '
cl-m4:8:14: too few arguments to builtin `index'
m4eof

:depends (list "builtin" "indir" "index")))


;;; 6 Conditionals, loops, and recursion
;; 6.1 Testing if a macro is defined
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

:error #>m4eof>cl-m4:5:53: excess arguments to builtin `ifdef' ignored
m4eof

:depends (list "ifdef" "define")))


;; 6.2 If-else construct, or multibranch
(deftest gnu-m4-6.2-1 ()
  (m4-test
#>m4>
ifelse(`some comments')
ifelse(`foo', `bar')
m4

#>m4>


m4

:error #>m4eof>cl-m4:3:20: too few arguments to builtin `ifelse'
m4eof

:depends (list "ifelse")))


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
m4

:depends (list "ifelse" "define")))


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
m4

:depends (list "ifelse" "define")))


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

:error #>m4eof>cl-m4:2:45: excess arguments to builtin `ifelse' ignored
cl-m4:5:56: excess arguments to builtin `ifelse' ignored
m4eof

:depends (list "ifelse" "define")))


;; 6.3 Recursion in m4
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
m4

:depends (list "shift")))


;;; 7 How to debug macros and input
;; 7.1 Displaying macro definitions
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
m4

:depends (list "define" "dumpdef")))


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
cl-m4:4:25: undefined macro `f'

m4eof

:depends (list "pushdef" "popdef" "dumpdef")))


;; TODO traceon, traceoff, debugmode, debugfile


;;; 8 Input control
;; 8.1 Deleting whitespace in input
(deftest gnu-m4-8.1-1 ()
  (m4-test
#>m4>
define(`foo', `Macro `foo'.')dnl A very simple macro, indeed.
foo
m4

#>m4>
Macro foo.
m4

:depends (list "define" "dnl")))


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

:error #>m4eof>cl-m4:3:27: excess arguments to builtin `dnl' ignored
m4eof

:depends (list "define" "dnl")))


(deftest gnu-m4-8.1-3 ()
  (m4-test
#>m4eof>
m4wrap(`m4wrap(`2 hi
')0 hi dnl 1 hi')
define(`hi', `HI')
m4eof

#>m4>


0 HI 2 HI
m4

:error #>m4eof>cl-m4:5:0: end of file treated as newline
m4eof

:depends (list "m4wrap" "define" "dnl")))


;; 8.2 Changing the quote characters
(deftest gnu-m4-8.2-1 ()
  (m4-test
#>m4>
changequote(`[', `]')
define([foo], [Macro [foo].])
foo
m4

#>m4>


Macro foo.
m4

:depends (list "define" "changequote")))


(deftest gnu-m4-8.2-2 ()
  (m4-test
#>m4>
changequote(`[[[', `]]]')
define([[[foo]]], [[[Macro [[[[[foo]]]]].]]])
foo
m4

#>m4>


Macro [[foo]].
m4

:depends (list "define" "changequote")))


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
m4

:depends (list "define" "changequote")))


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
m4

:depends (list "define" "changequote")))


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
m4

:depends (list "define" "changequote")))


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
m4

:depends (list "changequote" "dnl" "define")))


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
m4

:depends (list "define" "changequote")))


;; 8.3 Changing the comment delimiters
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
m4

:depends (list "define" "changecom")))


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
m4

:depends (list "define" "changecom")))


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
m4

:depends (list "define" "changecom")))


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
m4

:depends (list "define" "changecom")))


;; Won't implement: changeword


;; 8.5 Saving text until end of input
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
m4

:depends (list "define" "m4wrap")))


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


(deftest gnu-m4-8.5-3 ()
  (m4-test
#>m4eof>
define(`aa', `AA
')
m4wrap(`a')m4wrap(`a')
m4eof

#>m4>


AA
m4

:depends (list "define" "m4wrap")))


;;; 9 File inclusion
;; 9.1 Including named files
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

:error #>m4eof>cl-m4:2:15: cannot open `none': No such file or directory
cl-m4:3:9: cannot open `': No such file or directory
m4eof

:depends (list "include" "sinclude")))


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
:include-path (list (relative-pathname "fixtures/"))
:depends (list "define" "include")))


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
:include-path (list (relative-pathname "fixtures/"))
:depends (list "define" "include")))


;;; 10 Diverting and undiverting output
;; 10.1 Diverting output
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
m4

:depends (list "divert")))


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
m4

:depends (list "define" "divert" "m4wrap")))


(deftest gnu-m4-10.1-3 ()
  (m4-test
#>m4>
divert(`-1')
define(`foo', `Macro `foo'.')
define(`bar', `Macro `bar'.')
divert
m4

#>m4>

m4

:depends (list "define" "divert")))


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
m4

:depends (list "define" "divert" "ifelse" "builtin")))


;; 10.2 Undiverting output
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

m4

:depends (list "divert" "undivert")))


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
m4

:depends (list "divert" "undivert")))


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
m4

:depends (list "divert" "undivert")))


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
m4

:depends (list "divert" "undivert" "dnl")))


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

:include-path (list (relative-pathname "fixtures/"))
:depends (list "define" "undivert" "include")))


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

:include-path (list (relative-pathname "fixtures/"))
:depends (list "divert" "undivert" "dnl")))


;; 10.3 Diversion numbers
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
m4

:depends (list "divert" "divnum")))


;; 10.4 Discarding diverted text
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
m4

:depends (list "divert" "undivert")))


;;; 11 Macros for text handling
;; 11.1 Calculating length of strings
(deftest gnu-m4-11.1 ()
  (m4-test
#>m4>
len()
len(`abcdef')
m4

#>m4>
0
6
m4

:depends (list "len")))


;; 11.2 Searching for substrings
(deftest gnu-m4-11.2-1 ()
  (m4-test
#>m4>
index(`gnus, gnats, and armadillos', `nat')
index(`gnus, gnats, and armadillos', `dag')
m4

#>m4>
7
-1
m4

:depends (list "index")))


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

:error #>m4eof>cl-m4:2:12: too few arguments to builtin `index'
m4eof

:depends (list "index")))


;; 11.3 Searching for regular expressions
(deftest gnu-m4-11.3-1 ()
  (m4-test
#>m4>
regexp(`GNUs not Unix', `\<[a-z]\w+')
regexp(`GNUs not Unix', `\<Q\w*')
regexp(`GNUs not Unix', `\w\(\w+\)$', `*** \& *** \1 ***')
regexp(`GNUs not Unix', `\<Q\w*', `*** \& *** \1 ***')
m4

#>m4>
5
-1
*** Unix *** nix ***

m4

:depends (list "regexp")))


(deftest gnu-m4-11.3-2 ()
  (m4-test
#>m4>
regexp(`abc', `\(b\)', `\\\10\a')
regexp(`abc', `b', `\1\')
regexp(`abc', `\(\(d\)?\)\(c\)', `\1\2\3\4\5\6')
m4

#>m4>
\b0a

c
m4

:error #>m4eof>cl-m4:3:25: sub-expression 1 not present
cl-m4:3:25: trailing \ ignored in replacement
cl-m4:4:48: sub-expression 4 not present
cl-m4:4:48: sub-expression 5 not present
cl-m4:4:48: sub-expression 6 not present
m4eof

:depends (list "regexp")))


(deftest gnu-m4-11.3-3 ()
  (m4-test
#>m4>
regexp(`abc')
regexp(`abc', `')
regexp(`abc', `', `\\def')
m4

#>m4>
0
0
\def
m4

:error #>m4eof>cl-m4:2:13: too few arguments to builtin `regexp'
m4eof

:depends (list "regexp")))


;; 11.4 Extracting substrings
(deftest gnu-m4-11.4-1 ()
  (m4-test
#>m4>
substr(`gnus, gnats, and armadillos', `6')
substr(`gnus, gnats, and armadillos', `6', `5')
m4

#>m4>
gnats, and armadillos
gnats
m4

:depends (list "substr")))


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

:error #>m4eof>cl-m4:2:13: too few arguments to builtin `substr'
cl-m4:3:14: empty string treated as 0 in builtin `substr'
m4eof

:depends (list "substr")))


;; 11.5 Translating characters
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
m4

:depends (list "translit")))


(deftest gnu-m4-11.5-2 ()
  (m4-test
#>m4>
translit(`abc')
m4

#>m4>
abc
m4

:error #>m4eof>cl-m4:2:15: too few arguments to builtin `translit'
m4eof

:depends (list "translit")))


;; 11.6 Substituting text by regular expression
(deftest gnu-m4-11.6-1 ()
  (m4-test
#>m4>
patsubst(`GNUs not Unix', `^', `OBS: ')
patsubst(`GNUs not Unix', `\<', `OBS: ')
patsubst(`GNUs not Unix', `\w*', `(\&)')
patsubst(`GNUs not Unix', `\w+', `(\&)')
patsubst(`GNUs not Unix', `[A-Z][a-z]+')
patsubst(`GNUs not Unix', `not', `NOT\')
m4

#>m4>
OBS: GNUs not Unix
OBS: GNUs OBS: not OBS: Unix
(GNUs)() (not)() (Unix)()
(GNUs) (not) (Unix)
GN not 
GNUs NOT Unix
m4

:error #>m4eof>cl-m4:7:40: trailing \ ignored in replacement
m4eof

:depends (list "patsubst")))


(deftest gnu-m4-11.6-2 ()
  (m4-test
#>m4>
patsubst(`abc')
patsubst(`abc', `')
patsubst(`abc', `', `\\-')
m4

#>m4>
abc
abc
\-a\-b\-c\-
m4

:error #>m4eof>cl-m4:2:15: too few arguments to builtin `patsubst'
m4eof

:depends (list "patsubst")))

