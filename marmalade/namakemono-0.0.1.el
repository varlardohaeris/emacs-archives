;;; namakemono.el --- utility function set for namakemono

;; Copyright (C) 2012  podhmo

;; Author: podhmo  
;; Version: 0.0.1
;; Keywords: convenience, util, namakemono

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'cl)

;; debug
(defalias 'namakemono:print-function-default 'print)
(setq namakemono:print-function 'namakemono:print-function-default)

(defmacro tapp (exp)
  (let ((tmp (gensym)))
    `(let ((,tmp ,exp))
       (funcall namakemono:print-function ,tmp)
       ,tmp)))

;; hash-table
(defsubst hash-table-get (table k &optional default)
  (gethash k table default))

(defsubst hash-table-put (table k v)
  (puthash k v table))

(defun hash-table-keys (table)
  (loop for k being the hash-keys in table
        collect k))

(defun hash-table-values (table)
  (loop for v being the hash-values in table
        collect v))

(defun hash-table->alist (table)
  (loop for k being the hash-keys in table using (hash-value v)
        collect (cons k v)))

(defun hash-table-mapcar (fn table)
  (loop for k being the hash-keys in table using (hash-value v)
        collect (fn k v))))

;; 

(defmacro aif (test-form then-form &rest else-forms)
  "Anaphoric if. Temporary variable `it' is the result of test-form."
  (declare (indent 2)
           (debug (form form &rest form)))
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))

(defmacro and-let* (bindings &rest body)
  "imported from srfi-2"
  (declare (indent 1))
  (reduce #'(lambda (binding r)
              (let ((head (car binding)))
                (cond ((and (atom head) (symbolp head))
                       `(let (,binding)
                          (when ,head ,r)))
                      ((listp head)
                       `(when ,head ,r))
                      (t
                       (error "and-let*: invalid head %s" head)))))
          bindings :from-end t :initial-value `(progn ,@body)))


(defmacro let1 (var val &rest body)
  "imported from gauche"
  (declare (indent 2))
  `(let ((,var ,val))
     ,@body))

(defmacro rlet1 (var val &rest body)
  "imported from gauche"
  (declare (indent 2))
  `(let1 ,var ,val
     ,@body
     ,var))

(defmacro with-lexical-bindings (syms &rest body)
  (declare (indent 1))
  (let ((clauses (loop for sym in syms collect (\` ((\, sym) (\, sym))))))
    (\` (lexical-let ((\,@ clauses)) (\,@ body)))))


;;; define action
(defmacro def-toggle (name &rest body)
  (and-let* ((on-clause (aif (assoc-default :on body) `(progn ,@it)))
             (off-clause (aif (assoc-default :off body) `(progn ,@it)))
             (state (gensym)) (flag (gensym)))
    `(lexical-let (,state)
       (defun ,name (&optional ,flag) (interactive "P")
         (case ,flag
           ((1 t) (progn ,on-clause (setq ,state t)))
           ((-1) (progn ,off-clause (setq ,state nil)))
           (otherwise (,name (if (not ,state) 1 -1))))))))


;; file travarse
(defun directory-files2 (directory &optional full nosort)
  (directory-files directory full "^[^\\.]\\{1,2\\}" nosort))

(defun decompose-file-path (path) ;;util
  (let ((ext  (file-name-extension path))
        (basename (file-name-nondirectory path)))
    (if ext
        (values (file-name-directory path)
                (substring basename 0 (string-match (format "\\.%s$" ext) basename))
                (concat "." ext))
      (values (file-name-directory path) basename ""))))

(defun check-target-exist-in-path (path target &optional find-all-p)
  (destructuring-bind (head . words) (split-string path"/")
    (let ((candidates
           (loop for word in words
                 with acc = head
                 unless (string-equal "" word)
                 do (setq acc (concat acc "/" word))
                 and collect acc into result
                 finally return (nreverse (cons head result)))))
      (cond (find-all-p
             (lexical-let ((target target))
               (remove-if-not (lambda (path)
                                (file-exists-p (concat path "/" target)))
                              candidates)))
            (t
             (find target candidates
                   :test (lambda (target path)
                           (file-exists-p (concat path "/" target)))))))))

(defun target-in-path (target &optional find-all-p)
  (and-let* ((dir (current-directory)))
    (check-target-exist-in-path
     (file-truename dir) target find-all-p)))

(provide 'namakemono)

;;; namakemono.el ends here
