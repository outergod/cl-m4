dnl#-1
dnl`0
dnl_foo 1
dnl4 2
dnl'foo 3
dnl#foo 4
dnl`foo' 5
dnlfoo? 6
abc # def
abc dnl def
abc#def
abcdnldef
abcdnl
`abc'
dnl `abc'
# `abc'
#abc
#`abc'
format(`%d',14)
format()
spaces: format(`%d ?' ?   ?, 14 5)
noargs: format(,14)
test: format(# %d %d?
,14,18)
# format(`%d',14)
`format(`%d',14)'
breakpoint
`dnl foo'
`# foo'
``foo''
``foo'
test
``foo'''
```foo'''
`foo
bar'
``foo'bar`baz''
dnl
