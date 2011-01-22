;;;; cl-m4 - regex.lisp
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

;;; conditions
(define-condition regex-compilation-failure (error)
  ((message :initarg :message
            :reader  regex-compilation-failure-message)))

(defmethod print-object ((condition regex-compilation-failure) stream)
  (format stream "libc regex compiliation failure: ~a~%"
          (regex-compilation-failure-message condition)))

(define-condition regex-internal-error (error) ())
  

(defmacro with-pattern-buffer ((var pattern &optional (syntax (list +syntax-emacs+))) &body body)
  "with-pattern-buffer var pattern &optional (syntax (list +syntax-emacs+))) &body body => context

Create lexical context binding VAR to a compiled libc regex PATTERN-BUFFER
 (re_pattern_buffer) with dynamic allocation and SYNTAX.
In case of REGEX compilation failure a REGEX-COMPILATION-FAILURE condition
containing the original error message is signaled."
  (let ((ret (gensym)))
    `(with-foreign-object (,var 'pattern-buffer)
       (setf (foreign-slot-value ,var 'pattern-buffer 'buffer) (make-pointer 0)
             (foreign-slot-value ,var 'pattern-buffer 'allocated) +regs-unallocated+
             (foreign-slot-value ,var 'pattern-buffer 'fastmap) (make-pointer 0)
             (foreign-slot-value ,var 'pattern-buffer 'translate) (make-pointer 0)
             (foreign-slot-value ,var 'pattern-buffer 'syntax) (logior ,@syntax))
       (let ((,ret (compile-pattern ,pattern (length ,pattern) ,var)))
         (if ,ret
             (error 'regex-compilation-failure :message ,ret)
           ,@body)))))

(defun get-register (registers index)
  "get-register registers index => (start end)

Return START and END of register match # INDEX in libc regex REGISTERS
(re_registers)."
  (values
   (mem-aref
    (mem-aref (foreign-slot-pointer registers 'registers 'start) :pointer)
    :regoff index)
   (mem-aref
    (mem-aref (foreign-slot-pointer registers 'registers 'end) :pointer)
    :regoff index)))

(flet ((%search (buffer registers target-string start end)
         (let ((startpos (%regex-search buffer target-string (length target-string)
                                        start end registers)))
           (cond ((= -2 startpos) (error 'regex-internal-error))
                 ((= -1 startpos) nil)
                 (t (let* ((matches-count (1- (foreign-slot-value registers 'registers 'num-regs)))
                           (matches (make-array matches-count :element-type 'integer :adjustable nil)))
                      (dotimes (position matches-count)
                        (setf (svref matches position)
                              (multiple-value-list (get-register registers position))))
                      (values startpos matches)))))))

  (defun regex-search (regex target-string &key (start 0) (end (length target-string)))
    "regex-search regex target-string &key (start 0) (end (length target-string)) => (startpos registers)

High-level interface to libc regex re_search. Match REGEX against region between
START and END of TARGET-STRING returning STARTPOS of the first match and
SIMPLE-VECTOR REGISTERS containing LISTs of register group STARTs and ENDs. The
first register always contains the match of the whole REGEX.
If no part of TARGET-STRING matches, nil is return.
In case of a libc internal error signal a REGEX-INTERNAL-ERROR condition."
    (with-pattern-buffer (buffer regex)
      (with-foreign-object (registers 'registers)
        (%search buffer registers target-string start end))))

  (defun regex-search-all (regex target-string &key (start 0) (end (length target-string)))
    "regex-search-all regex target-string &key (start 0) (end (length target-string)) => list

Multi-version of REGEX-SEARCH that evaluates to a list of all (STARTPOS
REGISTERS) matches of REGEX in TARGET-STRING."
    (with-pattern-buffer (buffer regex)
      (with-foreign-object (registers 'registers)
        (labels ((rec (position acc)
                   (multiple-value-bind (startpos matches)
                       (%search buffer registers target-string position end)
                     (if startpos
                         (rec (+ startpos
                                 (abs (apply #'- (svref matches 0))))
                              (cons (cons startpos matches)
                                    acc))
                         (nreverse acc)))))
          (rec start (list)))))))

(defun regex-match (regex target-string &optional (start 0))
  "regex-match regex target-string &optional (start 0) => incomplete"
  (with-pattern-buffer (buffer regex)
    (with-foreign-object (registers 'registers)
      (%regex-match buffer target-string (length target-string)
                    start registers))))
