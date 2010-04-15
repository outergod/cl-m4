;;;; evol - m4-parser.lisp
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

(in-package :evol)

(define-condition m4-parse-error (error)
  ((message :initarg :message
            :reader m4-parse-error-message)
   (row   :initarg :row
          :reader m4-parse-error-row)
   (column :initarg :column
           :reader m4-parse-error-column)))

(defun split-merge (string-list split-string)
  (labels ((acc (rec string rest)
                (let ((char (car rest)))
                  (cond ((null char) (nreverse (cons string rec)))
                        ((not (stringp char)) ; macro-token
                         (acc (cons char rec) "" (cdr rest)))
                        ((string= split-string (car rest))
                         (acc (cons string rec) "" (cdr rest)))
                        (t (acc rec (concatenate 'string string char) (cdr rest)))))))
          (acc (list) "" string-list)))

(defvar *m4-quoting-level*)
(defvar *m4-string*)
(defvar *m4-macro-queue*)
(defvar *m4-parsing-row*)
(defvar *m4-parsing-column*)
                  
(defun call-m4-macro (macro args)
  (let ((macrofun (m4-macro macro)))
    (cond (macrofun (if args
                        (apply macrofun (mapcar #'(lambda (string)
                                                    (if (stringp string)
                                                        (string-left-trim " " string)
                                                      string)) ; macro-token
                                                (split-merge args ",")))
                      (funcall macrofun)))
          (args (format nil "~a(~{~a~})" macro args))
          (t macro))))

(defun m4-lexer (&optional (peek nil))
  (labels ((dynamic-scan (rules)
             (apply #'values
                    (some #'(lambda (pair)
                              (let ((match (cl-ppcre:scan-to-strings (concatenate 'string "^" (car pair)) *m4-string*)))
                                (when match
                                  (list (cdr pair) match (length match)))))
                          rules))))
    (if *m4-macro-queue*
        (values :macro-token (pop *m4-macro-queue*))
      (multiple-value-bind (class image remainder)
          (dynamic-scan `((,*m4-quote-start* . :quote-start)
                          (,*m4-quote-end* . :quote-end)
                          (,*m4-comment* . :comment)
                          (,*m4-macro-name* . :macro-name)
                          (" " . :space)
                          ("\\n" . :newline)
                          ("\\(" . :open-paren)
                          ("\\)" . :close-paren)
                          ("," . :comma)
                          ("." :token)))
        (when (and remainder (null peek))
          (if (equal :newline class)
              (progn
                (setq *m4-parsing-column* 0)
                (incf *m4-parsing-row*))
            (incf *m4-parsing-column* remainder))
          (setq *m4-string* (subseq *m4-string* remainder)))
        (values class image)))))

(defun parse-m4-comment (lexer image)
  (labels ((m4-comment (rec)
             (multiple-value-bind (class image)
                 (funcall lexer)
               (cond ((null class)
                      (apply #'concatenate 'string (nreverse rec)))
                     ((equal :newline class)
                      (apply #'concatenate 'string
                             (nreverse (cons image rec))))
                 (t (m4-comment (cons image rec)))))))
    (m4-comment (list image))))

(defun parse-m4-quote (lexer)
  (let ((row *m4-parsing-row*)
        (column *m4-parsing-column*))
    (labels ((m4-quote (rec quoting-level)
               (multiple-value-bind (class image)
                   (funcall lexer)
                 (cond ((null class)
                        (error 'm4-parse-error :message "EOF with unfinished quoting" :row row :column column))
                        ((equal :quote-end class)
                         (if (= 1 quoting-level)
                             (apply #'concatenate 'string (nreverse rec))
                           (m4-quote (cons image rec) (1- quoting-level))))
                        ((equal :quote-start class)
                         (m4-quote (cons image rec) (1+ quoting-level)))
                        (t (m4-quote (cons image rec) quoting-level))))))
      (m4-quote (list) 1))))

(defun parse-m4-macro-arguments (lexer)
  (let ((row *m4-parsing-row*)
        (column *m4-parsing-column*))
    (labels ((m4-macro-arguments (rec paren-level)
               (multiple-value-bind (class image)
                   (funcall lexer)
                 (cond ((null class)
                        (error 'm4-parse-error :message "EOF with unfinished argument list" :row row :column column))
                       ((equal :close-paren class)
                        (if (= 1 paren-level)
                            (nreverse rec)
                          (m4-macro-arguments (cons image rec) (1- paren-level))))
                       ((equal :open-paren class)
                        (m4-macro-arguments (cons image rec) (1+ paren-level)))
                       ((equal :quote-start class)
                        (m4-macro-arguments (cons (parse-m4-quote lexer) rec) paren-level))
                       ((equal :comment class)
                        (m4-macro-arguments (cons (parse-m4-comment lexer image) rec) paren-level))
                       ((equal :macro-name class)
                        (m4-macro-arguments (cons (parse-m4-macro lexer image) rec) paren-level))
                       (t (m4-macro-arguments (cons image rec) paren-level))))))
      (m4-macro-arguments (list "") 1))))

(defun parse-m4-dnl (lexer)
  (do ((class (funcall lexer) (funcall lexer)))
      ((or (equal :newline class)
           (null class)) "")))

(defun parse-m4-macro (lexer macro-name)
  (handler-case
   (multiple-value-bind (class image)
       (funcall lexer t)
     (declare (ignore image))
     (if (equal :open-paren class)
         (progn
           (funcall lexer) ; consume token
           (call-m4-macro macro-name (parse-m4-macro-arguments lexer)))
       (call-m4-macro macro-name nil)))
   (macro-dnl-invocation-condition ()
     (parse-m4-dnl lexer))
   (macro-defn-invocation-condition (condition)
     (setq *m4-macro-queue* (macro-defn-invocation-result condition))
     "")
   (macro-invocation-condition (condition)
     (setq *m4-string* (concatenate 'string
                                    (macro-invocation-result condition)
                                    *m4-string*))
     "")))

(defun parse-m4 (lexer)
  (labels ((m4 (rec)
             (multiple-value-bind (class image)
                 (funcall lexer)
               (cond ((null class)
                     (apply #'concatenate 'string (nreverse rec)))
                     ((equal :quote-start class)
                      (m4 (cons (parse-m4-quote lexer) rec)))
                     ((equal :comment class)
                      (m4 (cons (parse-m4-comment lexer image) rec)))
                     ((equal :macro-name class)
                      (m4 (cons (parse-m4-macro lexer image) rec)))
                     ((equal :macro-token class)
                      (m4 (cons "" rec)))
                     (t (m4 (cons image rec)))))))
    (m4 (list))))

(defun process-m4 (string)
  (let ((*m4-quoting-level* 0)
        (*m4-string* (copy-seq string))
        (*m4-macro-queue* nil)
        (*m4-parsing-row* 1)
        (*m4-parsing-column* 0)
        (*m4-quote-start* "`")
        (*m4-quote-end* "'")
        (*m4-comment* "#")
        (*m4-macro-name* "[_a-zA-Z]\\w*"))
    (with-m4-lib (parse-m4 #'m4-lexer))))
