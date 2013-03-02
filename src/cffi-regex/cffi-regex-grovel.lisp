;;;; cl-m4 - cffi-regex-grovel.lisp
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

(in-package :cffi-regex)

;;; This is part of glibc regex.h expressed in cffi-grovel DSL and oughts to
;;; ensure portability between platforms (hopefully).

; regex.h requires __USE_GNU to be defined for the same feature set as GNU m4;
; on some platforms features.h gets sucked in early resulting in a reset of
; __USE_GNU to undef unless _GNU_SOURCE has been defined, hence features.h is
; explicitly included to ensure __USE_GNU being set during regex.h processing.
(define "_GNU_SOURCE" 1)
(include "features.h")
(include "regex.h")
(include "string.h")

;; c typdefs
(ctype size-t "size_t")
(ctype s-reg "s_reg_t")
(ctype active-reg "active_reg_t")
(ctype reg-syntax "reg_syntax_t")
(ctype regoff "regoff_t")

;; option constants
(constant (+backslash-escape-in-lists+ "RE_BACKSLASH_ESCAPE_IN_LISTS"))
(constant (+bk-plus-qm+ "RE_BK_PLUS_QM"))
(constant (+char-classes+ "RE_CHAR_CLASSES"))
(constant (+context-indep-anchors+ "RE_CONTEXT_INDEP_ANCHORS"))
(constant (+context-indep-ops+ "RE_CONTEXT_INDEP_OPS"))
(constant (+context-invalid-ops+ "RE_CONTEXT_INVALID_OPS"))
(constant (+dot-newline+ "RE_DOT_NEWLINE"))
(constant (+dot-not-null+ "RE_DOT_NOT_NULL"))
(constant (+hat-lists-not-newline+ "RE_HAT_LISTS_NOT_NEWLINE"))
(constant (+intervals+ "RE_INTERVALS"))
(constant (+limited-ops+ "RE_LIMITED_OPS"))
(constant (+newline-alt+ "RE_NEWLINE_ALT"))
(constant (+no-bk-braces+ "RE_NO_BK_BRACES"))
(constant (+no-bk-parens+ "RE_NO_BK_PARENS"))
(constant (+no-bk-refs+ "RE_NO_BK_REFS"))
(constant (+no-bk-vbar+ "RE_NO_BK_VBAR"))
(constant (+no-empty-ranges+ "RE_NO_EMPTY_RANGES"))
(constant (+unmatched-right-paren-ord+ "RE_UNMATCHED_RIGHT_PAREN_ORD"))
(constant (+no-posix-backtracking+ "RE_NO_POSIX_BACKTRACKING"))
(constant (+no-gnu-ops+ "RE_NO_GNU_OPS"))
(constant (+debug+ "RE_DEBUG"))
(constant (+invalid-interval-ord+ "RE_INVALID_INTERVAL_ORD"))
(constant (+re-icase+ "RE_ICASE"))
(constant (+caret-anchors-here+ "RE_CARET_ANCHORS_HERE"))
(constant (+context-invalid-dup+ "RE_CONTEXT_INVALID_DUP"))
(constant (+no-sub+ "RE_NO_SUB"))

;; syntax constants
(constant (+syntax-emacs+ "RE_SYNTAX_EMACS"))
(constant (+syntax-awk+ "RE_SYNTAX_AWK"))
(constant (+syntax-gnu-awk+ "RE_SYNTAX_GNU_AWK"))
(constant (+syntax-posix-awk+ "RE_SYNTAX_POSIX_AWK"))
(constant (+syntax-grep+ "RE_SYNTAX_GREP"))
(constant (+syntax-egrep+ "RE_SYNTAX_EGREP"))
(constant (+syntax-posix-egrep+ "RE_SYNTAX_POSIX_EGREP"))
(constant (+syntax-ed+ "RE_SYNTAX_ED"))
(constant (+syntax-sed+ "RE_SYNTAX_SED"))
(constant (+syntax-posix-common+ "_RE_SYNTAX_POSIX_COMMON"))
(constant (+syntax-posix-basic+ "RE_SYNTAX_POSIX_BASIC "))
(constant (+syntax-posix-minimal-basic+ "RE_SYNTAX_POSIX_MINIMAL_BASIC"))
(constant (+syntax-posix-extended+ "RE_SYNTAX_POSIX_EXTENDED"))
(constant (+syntax-posix-minimal-extended+ "RE_SYNTAX_POSIX_MINIMAL_EXTENDED"))

;; max number of duplicates constant
(constant (+dup-max+ "RE_DUP_MAX"))

;; cflags constants
(constant (+extended+ "REG_EXTENDED"))
(constant (+reg-icase+ "REG_ICASE"))
(constant (+newline+ "REG_NEWLINE"))
(constant (+nosub+ "REG_NOSUB"))

;; eflags constants
(constant (+notbol+ "REG_NOTBOL"))
(constant (+noteol+ "REG_NOTEOL"))
(constant (+startend+ "REG_STARTEND"))

;; enums
(cenum errcode
  ((:noerror "REG_NOERROR"))
  ((:nomatch "REG_NOMATCH"))
  ((:badpat "REG_BADPAT"))
  ((:ecollate "REG_ECOLLATE"))
  ((:ectype "REG_ECTYPE"))
  ((:eescape "REG_EESCAPE"))
  ((:esubreg "REG_ESUBREG"))
  ((:ebrack "REG_EBRACK"))
  ((:eparen "REG_EPAREN"))
  ((:ebrace "REG_EBRACE"))
  ((:badbr "REG_BADBR"))
  ((:erange "REG_ERANGE"))
  ((:espace "REG_ESPACE"))
  ((:badrpt "REG_BADRPT"))
  ((:eend "REG_EEND"))
  ((:esize "REG_ESIZE"))
  ((:erparen "REG_ERPAREN")))

;; pattern buffer
(constant (+regs-unallocated+ "REGS_UNALLOCATED"))
(constant (+regs-reallocate+ "REGS_REALLOCATE"))
(constant (+regs-fixed+ "REGS_FIXED"))

(cstruct pattern-buffer "regex_t"
  (buffer "buffer" :type :string)
  (allocated "allocated" :type :long)
  (used "used" :type :long)
  (syntax "syntax" :type reg-syntax)
  (fastmap "fastmap" :type :pointer)
  (translate "translate" :type :pointer)
  (nsub "re_nsub" :type size-t))
