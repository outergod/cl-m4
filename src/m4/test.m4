dnl_with_args:                 dnl(foo)
(blank_line)
dnl_with_empty_args:           dnl()
(blank_line)
dnl_quote_start:               dnl`0
(blank_line)
dnl_underscore:                dnl_foo 1
(blank_line)
dnl_number:                    dnl4 2
(blank_line)
dnl_quote_end:                 dnl'foo 3
(blank_line)
dnl_comment:                   dnl#foo 4
(blank_line)
dnl_quotes:                    dnl`foo' 5
(blank_line)
dnl_word:                      dnlfoo? 6
(blank_line)
name_comment_space:            abc # def
name_dnl_space:                abc dnl def
(blank_line)
name_comment_nospace:          abc#def
name_cnl_nospace:              abcdnldef
(blank_line)
name_dnl_newline:              abcdnl
(blank_line)
quoted_string:                 `abc'
dnl_quoted_string_space:       dnl `abc'
(blank_line)
comment_quoted_string:         # `abc'
comment_name:                  #abc
comment_quoted_string_nospace: #`abc'
macro_call:                    format(`%d',14)
macro_call_emptyargs:          format()
macro_call_spaces:             format(`%d ?' ?   ?, 14 5)
macro_call_argempty:           format(,14)
macro_call_comment_newline:    format(# %d %d?
,14,18)
comment_macro_call:            # format(`%d',14)
quoted_macro_call:             `format(`%d',14)'
quoted_dnl:                    `dnl foo'
quoted_comment:                `# foo'
double_quote:                  ``foo''
multi_quote_newline:           ``foo'
test
``foo'''
(multi_quote_end)
triple_quote:                  ```foo'''
single_quote_newline:
`foo
bar'
single_quote_newline_end
cascaded_quote:                ``foo'bar`baz''
number_followed_macro_name:    4format(`%d',14)
number_followed_dnl:           4dnl foo
(blank_line)
underscore_followed_dnl:       _dnl foo
macro_followed_string:         format(`%d',14):
gnu_m4_man_4.4:                foo(() (`(') `(')
dnl_at_end:                    dnl
