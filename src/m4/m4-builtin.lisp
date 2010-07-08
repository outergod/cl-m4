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

;;; M4 builtin macros and their helpers ahead.


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
  (cl-ppcre:regex-replace-all "(.-.)" string
                              (replace-with-region
                               #'(lambda (match)
                                   (let* ((start (char-code (schar match 0)))
                                          (end   (char-code (schar match 2)))
                                          (step  (if (>= end start) 1 -1))
                                          (bag (make-string (1+ (abs (- end start))))))
                                     (do* ((index 0 (+ step index))
                                           (char (code-char start) (code-char (+ index start))))
                                         ((= (+ step end) (+ index start)) bag)
                                       (setf (schar bag (abs index)) char)))))))

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

(defun pushm4macro (name fun &optional (replace t))
  (let ((stack (gethash name *m4-runtime-lib*)))
    (if stack
        (if replace
            (setf (aref stack (1- (fill-pointer stack))) fun)
          (vector-push-extend fun stack))
      (setf (gethash name *m4-runtime-lib*)
            (make-array 1 :adjustable t :fill-pointer 1
                          :initial-contents
                          (list fun))))))

(defun popm4macro (name)
  (let ((stack (gethash name *m4-runtime-lib*)))
    (when stack
      (if (> (fill-pointer stack) 1)
          (vector-pop stack)
        (remhash name *m4-runtime-lib*)))))

(defmacro defm4macro (name args (&key (arguments-only t) (minimum-arguments 0)) &body body)
  (let ((macro-args (gensym))
        (ignored-rest (gensym))
        (internal-call (gensym)))
    `(setf (gethash ,name *m4-lib*)
           (make-array 1 :adjustable t :fill-pointer 1
                         :initial-contents
                         (list #'(lambda (,internal-call &rest ,macro-args)
                                   (cond ((eql :definition ,internal-call)
                                          (concatenate 'string "<" ,name ">"))
                                         ((and ,arguments-only (not ,internal-call) (null ,macro-args)) ; most macros are only recognized with parameters
                                          ,name)
                                         ((< (length ,macro-args) ,minimum-arguments)
                                          (m4-warn (format nil "too few arguments to builtin `~a'" ,name))
                                          "")
                                         (t ,(if (member '&rest args)
                                                 `(destructuring-bind ,args ,macro-args
                                                    ,@body)
                                               `(destructuring-bind (,@args &rest ,ignored-rest) ,macro-args
                                                  (when ,ignored-rest
                                                    (m4-warn (format nil "excess arguments to builtin `~a' ignored" ,name)))
                                                  ,@body))))))))))

(defun defm4runtimemacro (name expansion &optional (replace t))
  (let ((fun (if (macro-token-p expansion)
                 (macro-token-m4macro expansion)
               #'(lambda (internal-call &rest macro-args)
                   (if (eql :definition internal-call)
                       expansion
                     (macro-return
                      (cl-ppcre:regex-replace-all "\\$(\\d+|#|\\*|@)" expansion
                                                  (replace-with-region
                                                   #'(lambda (match)
                                                       (cond ((string= "#" match)
                                                              (write-to-string (length macro-args)))
                                                             ((string= "*" match)
                                                              (format nil "~{~a~^,~}" macro-args))
                                                             ((string= "@" match)
                                                              (format nil
                                                                      (concatenate 'string "~{" (m4-quote-string "~a") "~^,~}")
                                                                      macro-args))
                                                             (t (let ((num (parse-integer match)))
                                                                  (if (= 0 num)
                                                                      name
                                                                    (or (nth (1- num) macro-args) ""))))))))))))))
    (pushm4macro name fun replace)))

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


;; m4 macro implementations
(defm4macro "define" (name &optional (expansion "")) (:minimum-arguments 1)
  (prog1 ""
    (when (string/= "" name)
      (defm4runtimemacro name expansion))))

(defm4macro "undefine" (&rest args) ()
  (prog1 ""
    (mapc #'(lambda (name)
              (remhash name *m4-runtime-lib*))
          args)))

(defm4macro "defn" (&rest args) ()
  (error 'macro-defn-invocation-condition
         :macros (mapcar #'(lambda (name)
                             (if (m4-macro name)
                                 (make-macro-token (m4-macro name)
                                                   (if (gethash name *m4-lib*)
                                                       ""
                                                     name))
                               ""))
                         args)))

(defm4macro "pushdef" (name &optional (expansion "")) (:minimum-arguments 1)
  (prog1 ""
    (when (string/= "" name)
      (defm4runtimemacro name expansion nil))))

(defm4macro "popdef" (&rest args) ()
  (prog1 ""
    (mapc #'popm4macro args)))

(defm4macro "indir" (name &rest args) (:minimum-arguments 1)
  (let ((macro (m4-macro name)))
    (cond ((null macro)
           (m4-warn (format nil "undefined macro `~a'" name))
           "")
          ((null args)
           (funcall macro t))
          (t (apply macro t args)))))

(defm4macro "builtin" (name &rest args) (:minimum-arguments 1)
  (let ((macro (m4-macro name t)))
    (cond ((null macro)
           (m4-warn (format nil "undefined builtin `~a'" name))
           "")
          ((null args)
           (funcall macro t))
          (t (apply macro t args)))))

(defm4macro "ifdef" (name string-1 &optional (string-2 "")) (:minimum-arguments 2)
  (macro-return
   (if (m4-macro name)
       string-1
     string-2)))

(defm4macro "ifelse" (&rest args) ()
  (labels ((ifelse (string-1 string-2 then &rest else)
             (cond ((string= string-1 string-2)
                    (macro-return then))
                   ((= 2 (list-length else))
                    (m4-warn "excess arguments to builtin `ifelse' ignored")
                    (macro-return (car else)))
                   ((> (list-length else) 1)
                    (apply #'ifelse else))
                   (t (macro-return (car else))))))
    (let ((num-args (list-length args)))
      (cond ((= 1 num-args) "") ; "Used with only one argument, the ifelse
                                ;  simply discards it and produces no output"
            ((= 2 num-args)
             (m4-warn "too few arguments to builtin `ifelse'")
             "")
            ((< num-args 5)
             (ifelse (car args) (cadr args) (caddr args) (or (cadddr args) "")))
            ((= 5 num-args) ; "If called with three or four arguments...A final
                            ;  fifth argument is ignored, after triggering a warning"
             (m4-warn "excess arguments to builtin `ifelse' ignored")
             (ifelse (car args) (cadr args) (caddr args) (cadddr args)))
            (t (apply #'ifelse (car args) (cadr args) (caddr args) (cdddr args)))))))

(defm4macro "shift" (&rest args) ()
  (macro-return (format nil (concatenate 'string "~{" (m4-quote-string "~a") "~^,~}") (cdr args))))

(defm4macro "dumpdef" (&rest args) (:arguments-only nil)
  (prog1 ""
    (dolist (name (sort (mapcar #'(lambda (name)
                                    (let ((macro (m4-macro name)))
                                      (if macro
                                          (format nil "~a:~a~a" name #\tab (funcall macro :definition))
                                        (progn
                                          (m4-warn (format nil "undefined macro `~a'" name))
                                          ""))))
                                (or args
                                    (alexandria:hash-table-keys *m4-runtime-lib*)))
                        #'string<))
      (format *error-output* "~a~%" name))))

;; TODO traceon, traceoff, debugmode, debugfile

(defm4macro "dnl" () (:arguments-only nil)
  (error 'macro-dnl-invocation-condition))

(defm4macro "changequote" (&optional (start "`") (end "'")) (:arguments-only nil)
  (prog1 ""
    (let ((end (if (string= "" end) "'" end)))
      (setq *m4-quote-start* (quote-regexp start)
            *m4-quote-end* (quote-regexp end)))))

(defm4macro "changecom" (&optional (start "") (end (string #\newline))) (:arguments-only nil)
  (prog1 ""
    (let ((end (if (string= "" end) (string #\newline) end)))
      (setq *m4-comment-start* (quote-regexp start)
            *m4-comment-end* (quote-regexp end)))))

(defm4macro "m4wrap" (&rest strings) (:minimum-arguments 1)
  (prog1 ""
    (push (format nil "~{~a~^ ~}" strings) *m4-wrap-stack*)))

(labels ((m4-include-file (file original-arg)
           (cond ((or (string= "" original-arg)
                      (not (cl-fad:file-exists-p file)))
                  (format nil "cannot open `~a': No such file or directory" original-arg))
                 ((cl-fad:directory-exists-p file)
                  (format nil "cannot open `~a': Is a directory" original-arg))
                 (t (handler-case (macro-return (with-open-file (stream file)
                                                  (let ((string (make-string (file-length stream))))
                                                    (read-sequence string stream)
                                                    string)))
                      (macro-invocation-condition (condition)
                        (error condition))
                      (condition ()
                        (format nil "cannot open `~a': Permission denied" original-arg))))))
         (m4-include (path warnfn)
           (prog1 ""
             (funcall warnfn 
               (if (eql :absolute (car (pathname-directory path)))
                   (m4-include-file path path)
                 (or (some #'(lambda (include-path)
                                 (prog1 nil (m4-include-file (merge-pathnames path include-path) path)))
                           *m4-include-path*)
                     (m4-include-file (merge-pathnames path) path)))))))

  (defm4macro "include" (file) (:minimum-arguments 1)
    (m4-include file #'m4-warn))

  (defm4macro "sinclude" (file) (:minimum-arguments 1)
    (m4-include file #'identity))

  (defm4macro "undivert" (&rest diversions) (:arguments-only nil)
    (apply #'concatenate 'string
           (if diversions
               (mapcar #'(lambda (diversion)
                           (handler-case
                               (let ((parsed-number (if (string= "" diversion)
                                                        0 ; "Undiverting the empty string is the same as specifying diversion 0"
                                                      (parse-integer diversion :junk-allowed nil))))
                                 (car (flush-m4-diversions parsed-number)))
                             (condition ()
                               (handler-case (m4-include diversion #'m4-warn)
                                 (macro-invocation-condition (condition)
                                   (macro-invocation-result condition))))))
                       diversions)
             (flush-m4-diversions)))))

(defm4macro "divert" (&optional (number "0")) (:arguments-only nil)
  (prog1 ""
    (handler-case
     (let ((parsed-number (parse-integer number :junk-allowed nil)))
       (unless (minusp parsed-number)
         (set-m4-diversion parsed-number))
       (setq *m4-diversion* parsed-number))
     (condition ()
       (m4-warn "non-numeric argument to builtin `divert'")))))

(defm4macro "divnum" () (:arguments-only nil)
  (write-to-string *m4-diversion*)) ; What happens if changeword is enabled and integer-only macro
                                    ; names have been allowed??

(defm4macro "len" (string) (:minimum-arguments 1)
  (write-to-string (length string)))

(defm4macro "index" (string &optional substring) (:minimum-arguments 1)
  (if substring
      (write-to-string (or (search substring string) -1))
    (prog1 "0" (m4-warn "too few arguments to builtin `index'"))))

(defm4macro "regexp" (string &optional regexp replacement) (:minimum-arguments 1)
  (if regexp
      (handler-case
       (multiple-value-bind (startpos registers)
           (regex-search regexp string)
         (if startpos
             (if replacement
                 (let ((replace-result (m4-regex-replace replacement string registers)))
                   (if (string= "\\" (subseq replace-result (1- (length replace-result))))
                       (prog1 (subseq replace-result 0 (1- (length replace-result)))
                         (m4-warn "trailing \\ ignored in replacement"))
                     replace-result))
               (write-to-string startpos))
           (if replacement "" "-1")))
       (regex-compilation-failure (condition)
         (m4-warn (format nil "bad regular expression: `~a': ~a"
                          regexp condition))
         "0")
       (regex-internal-error ()
         (m4-warn (format nil "error matching regular expression `~a'" regexp))
         "0"))
    (prog1 "0" (m4-warn "too few arguments to builtin `regexp'"))))

(defm4macro "substr" (string &optional from length) (:minimum-arguments 1)
  (if from
      (flet ((string-or-0 (string)
               (if (string= "" string)
                   (prog1 "0"
                     (m4-warn "empty string treated as 0 in builtin `substr'"))
                 string)))
        (handler-case
         (let* ((start (parse-integer (string-or-0 from) :junk-allowed nil))
                (parsed-length (and length
                                    (parse-integer (string-or-0 length) :junk-allowed nil)))
                (end (if (or (not parsed-length)
                             (> (+ start parsed-length) (length string)))
                         (length string)
                       (+ start parsed-length))))
           (if (< -1 start end)
               (macro-return (subseq string start end))
             ""))
         (macro-invocation-condition (condition)
           (error condition))
         (condition ()
           (m4-warn "non-numeric argument to builtin `substr'")
           "")))
    (progn
      (m4-warn "too few arguments to builtin `substr'")
      (macro-return string))))

(defm4macro "translit" (string &optional chars replacement) (:minimum-arguments 1)
  (if chars
      (macro-return (translate string chars replacement))
    (progn
      (m4-warn "too few arguments to builtin `translit'")
      (macro-return string))))
