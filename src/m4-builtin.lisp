;;;; cl-m4 - m4-builtin.lisp
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

(in-package :cl-m4)

;;; M4 builtin macros ahead.
;; internal functions

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

(defmacro defm4macro (name args (&key (arguments-only t) (minimum-arguments 0) (accept-macro-tokens nil)) &body body)
  (let ((macro-name (gensym))
        (macro-args (gensym))
        (ignored-rest (gensym))
        (internal-call (gensym)))
    (flet ((transform-arguments (args)
             (if accept-macro-tokens
                 args
               `(mapcar #'(lambda (arg)
                            (if (stringp arg) arg ""))
                        ,args))))
      `(setf (gethash ,name *m4-lib*)
             (make-array 1 :adjustable t :fill-pointer 1
                           :initial-contents
                           (list #'(lambda (,macro-name ,internal-call &rest ,macro-args)
                                     (declare (ignore ,macro-name))
                                     (cond ((eql :definition ,internal-call)
                                            (concatenate 'string "<" ,name ">"))
                                           ((eql :expansion ,internal-call)
                                            "")
                                           ((and ,arguments-only (not ,internal-call) (null ,macro-args)) ; most macros are only recognized with parameters
                                            ,name)
                                           ((< (length ,macro-args) ,minimum-arguments)
                                            (m4-warn (format nil "too few arguments to builtin `~a'" ,name))
                                            "")
                                           (t ,(if (member '&rest args)
                                                   `(destructuring-bind ,args
                                                        ,(transform-arguments macro-args)
                                                      ,@body)
                                                 `(destructuring-bind (,@args &rest ,ignored-rest)
                                                      ,(transform-arguments macro-args)
                                                    (when ,ignored-rest
                                                      (m4-warn (format nil "excess arguments to builtin `~a' ignored" ,name)))
                                                    ,@body)))))))))))

(defun defm4runtimemacro (name expansion &optional (replace t))
  (let ((fun (if (macro-token-p expansion)
                 (macro-token-m4macro expansion)
               #'(lambda (macro-name internal-call &rest macro-args)
                   (if (find internal-call '(:definition :expansion))
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
                                                                      macro-name
                                                                    (or (nth (1- num) macro-args) ""))))))))))))))
    (pushm4macro name fun replace)))


;; m4 macro implementations
(defm4macro "define" (name &optional (expansion "")) (:minimum-arguments 1 :accept-macro-tokens t)
  (if (stringp name)
      (prog1 ""
        (when (string/= "" name)
          (defm4runtimemacro name expansion)))
    (prog1 "" (m4-warn "define: invalid macro name ignored"))))

(defm4macro "undefine" (&rest args) ()
  (prog1 ""
    (mapc #'(lambda (name)
              (remhash name *m4-runtime-lib*))
          args)))

(defm4macro "defn" (&rest args) ()
  (cond ((= 0 (length args))
         "")
        ((and (= 1 (length args))
              (m4-macro (car args) t)) ; builtin macro
         (error 'macro-defn-invocation-condition
                :macro (make-macro-token (m4-macro (car args) t) (car args))))
        (t (macro-return
            (apply #'concatenate 'string            
                   (mapcar #'(lambda (name)
                               (let ((macro (m4-macro name)))
                                 (if macro
                                     (if (m4-macro name t)
                                         (prog1 ""
                                           (m4-warn (format nil "cannot concatenate builtin `~a'" name)))
                                       (m4-quote-string (funcall macro name :expansion)))
                                   "")))
                           args))))))

(defm4macro "pushdef" (name &optional (expansion "")) (:minimum-arguments 1)
  (prog1 ""
    (when (string/= "" name)
      (defm4runtimemacro name expansion nil))))

(defm4macro "popdef" (&rest args) ()
  (prog1 ""
    (mapc #'popm4macro args)))

(defm4macro "indir" (name &rest args) (:minimum-arguments 1 :accept-macro-tokens t)
  (if (stringp name)
      (let ((macro (m4-macro name)))
        (cond ((null macro)
               (m4-warn (format nil "undefined macro `~a'" name))
               "")
              ((null args)
               (funcall macro name t))
              (t (apply macro t name args))))
    (prog1 "" (m4-warn "indir: invalid macro name ignored"))))

(defm4macro "builtin" (name &rest args) (:minimum-arguments 1 :accept-macro-tokens t)
  (if (stringp name)
      (let ((macro (m4-macro name t)))
        (cond ((null macro)
               (m4-warn (format nil "undefined builtin `~a'" name))
               "")
              ((null args)
               (funcall macro name t))
              (t (apply macro name t args))))
    (prog1 "" (m4-warn "builtin: invalid macro name ignored"))))
  
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
                                          (format nil "~a:~a~a" name #\tab (funcall macro name :definition))
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
  (flet ((set-diversion (string)
           (handler-case
            (let ((parsed-number (parse-integer string :junk-allowed nil)))
              (unless (minusp parsed-number)
                (set-m4-diversion parsed-number))
              (setq *m4-diversion* parsed-number))
            (condition ()
              (m4-warn "non-numeric argument to builtin `divert'")))))
    (prog1 ""
      (set-diversion (if (or (not (stringp number))
                             (string= "" number))
                         (prog1 "0" (m4-warn "empty string treated as 0 in builtin `divert'"))
                       number)))))

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
