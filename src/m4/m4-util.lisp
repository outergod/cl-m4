;;;; evol - m4-builtin.lisp
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

;;; M4 helpers ahead.
;; conditions & structs
(defstruct (macro-token (:constructor make-macro-token (m4macro name)))
  m4macro name)

(define-condition macro-invocation-condition (error)
  ((result :initarg :result
           :reader macro-invocation-result)))

(define-condition macro-dnl-invocation-condition (error) ())

(define-condition macro-defn-invocation-condition (error)
  ((macros :initarg :macros
           :reader macro-defn-invocation-result)))


;; utilities
(defun quote-regexp (string)
  (let ((quote-charbag "\\()^$[]{}*?.")
        (quoted-string (make-array (length string) :adjustable t :fill-pointer 0)))
    (map 'nil #'(lambda (char)
                  (when (find char quote-charbag)
                    (vector-push-extend #\\ quoted-string))
                  (vector-push-extend char quoted-string))
         string)
    (coerce quoted-string 'string)))

(defun unquote-regexp (string)
  (let ((quote-charbag "\\()^$[]{}*?.")
        (char-list (coerce string 'list)))
    (labels ((acc (rec char rest)
               (cond ((null rest)
                      (coerce (nreverse (cons char rec)) 'string))
                     ((and (string= "\\" char)
                           (find (car rest) quote-charbag))
                      (acc rec (car rest) (cdr rest)))
                     (t (acc (cons char rec) (car rest) (cdr rest))))))
      (if (null char-list)
          ""
        (acc (list) (car char-list) (cdr char-list))))))

(defun expand-ascii-ranges (string)
  (labels ((acc (rec start-region end-region offset)
               (if (= start-region end-region)
                   (apply #'concatenate 'string (nreverse rec))
                 (let* ((start (+ (char-code (schar string start-region))
                                  offset))
                        (next  (+ 2 start-region))
                        (end   (char-code (schar string next)))
                        (step  (if (>= end start) 1 -1))
                        (bag (make-string (1+ (abs (- end start))))))
                   (acc (cons (do* ((index 0 (+ step index))
                                    (char (code-char start) (code-char (+ index start))))
                                  ((= (+ step end) (+ index start)) bag)
                                (setf (schar bag (abs index)) char))
                              rec)
                        next end-region (* -1 step))))))
    (cl-ppcre:regex-replace-all ".(?:-.)+" string
                                #'(lambda (target-string start end match-start match-end reg-starts reg-ends)
                                    (declare (ignore target-string start end reg-starts reg-ends))
                                    (acc (list) match-start (1- match-end) 0)))))

(defun translate (string charbag &optional (replacebag ""))
  (let ((charbag    (expand-ascii-ranges charbag))
        (replacebag (expand-ascii-ranges replacebag)))
    (apply #'concatenate 'string
           (mapcar #'(lambda (char)
                       (let ((pos (position char charbag)))
                         (if pos
                             (if (< pos (length replacebag))
                                 (string (schar replacebag pos))
                               "")
                           (string char))))
                   (coerce string 'list)))))


;; dynamic variables
(defparameter *m4-lib* (make-hash-table :test #'equal))
(defvar *m4-runtime-lib*)
(defvar *m4-quote-start*)
(defvar *m4-quote-end*)
(defvar *m4-comment-start*)
(defvar *m4-comment-end*)
(defvar *m4-macro-name*)
(defvar *m4-wrap-stack*)
(defvar *m4-include-path*)
(defvar *m4-diversion*)
(defvar *m4-diversion-table*)
(defvar *m4-parse-row*)
(defvar *m4-parse-column*)


;; internal functions
(defun m4-warn (datum)
  (flet ((boundp-or-? (var)
           (if (boundp var)
               (eval var)
             "?")))
    (format *error-output* "cl-m4:~a:~a: ~a~%" (boundp-or-? '*m4-parse-row*)
                                               (boundp-or-? '*m4-parse-column*)
                                               datum)))

(defun m4-quote-string (string)
  (concatenate 'string
               (unquote-regexp *m4-quote-start*)
               string
               (unquote-regexp *m4-quote-end*)))

(defun m4-regex-replace (template string registers)
  (flet ((nth-match (index)
           (if (< index (length registers))
               (let ((range (svref registers index)))
                 (if (>= (car range) 0) ; empty optional register groups are (-1 -1)
                     (apply #'subseq string range)
                   ""))
             (prog1 ""
               (m4-warn (format nil "sub-expression ~d not present" index))))))
    (cl-ppcre:regex-replace-all "\\\\(.)" template
                                (replace-with-region
                                 #'(lambda (match)
                                     (cond ((string= "\\" match) "\\")
                                           ((string= "&"  match) (nth-match 0))
                                           ((search match "0123456789")
                                            (nth-match (parse-integer match :junk-allowed nil)))
                                           (t match)))))))

(defun macro-return (result)
  (error 'macro-invocation-condition :result result))

(defun m4-macro (macro &optional (builtin nil))
  (let ((stack (gethash macro (if builtin *m4-lib* *m4-runtime-lib*))))
    (when stack
      (aref stack (1- (fill-pointer stack))))))

(defmacro with-m4-lib (&body body)
  `(let ((*m4-runtime-lib* (alexandria:copy-hash-table *m4-lib* :key #'copy-array)))
     ,@body))

(defun make-m4-diversion-table (stream)
  (let ((table (make-hash-table)))
    (prog1 table (setf (gethash 0 table) stream))))

(defun m4-diversion (&optional number)
  (gethash (or number *m4-diversion*) *m4-diversion-table*))

(defmacro with-m4-diversion-stream ((var &optional number) &body body)
  (let ((diversion (gensym)))
    `(let ((,diversion (or ,number *m4-diversion*)))
       (cond ((zerop ,diversion)
              (let ((,var (m4-diversion 0)))
                ,@body))
             ((minusp ,diversion)
              (with-open-file (,var "/dev/null" :direction :output :if-exists :append) ; TODO portability?
                ,@body))
             (t (with-output-to-string (,var (m4-diversion ,diversion))
                  ,@body))))))

(defun set-m4-diversion (number)
  (or (gethash number *m4-diversion-table*)
      (setf (gethash number *m4-diversion-table*)
            (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))))

(defun flush-m4-diversions (&rest diversions)
  (flet ((flush (diversion)
           (if (or (= *m4-diversion* diversion) ; "Attempts to undivert the current diversion are silently ignored"
                   (zerop diversion)) ""
             (prog1 (or (m4-diversion diversion) "")
               (remhash diversion *m4-diversion-table*)))))
    (mapcar #'flush (or diversions
                        (sort (alexandria:hash-table-keys *m4-diversion-table*) #'<)))))
