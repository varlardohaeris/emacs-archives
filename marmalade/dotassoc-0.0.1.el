;;; dotassoc.el --- dot access embedded alists

;; Copyright (C) 2012  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Maintainer: Nic Ferrier <nferrier@ferrier.me.uk>
;; Created: 14 August 2012
;; Version: 0.0.1
;; Keywords: lisp

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

;; Simple a.b.c access for nested alists

;;; Code:

(defun dotassoc-fn (expr table func)
  "Use the dotted EXPR to access deeply nested data in TABLE.

EXPR is a dot separated expression, either a symbol or a string.
For example:

 \"a.b.c\"

or:

 'a.b.c

If the EXPR is a symbol then the keys of the alist are also
expected to be symbols.

TABLE is expected to be an alist currently.

FUNC is some sort of `assoc' like function."
  (let ((state table)
        (parts
         (if (symbolp expr)
             (mapcar
              'intern
              (split-string (symbol-name expr) "\\."))
             ;; Else it's a string
             (split-string expr "\\."))))
    (catch 'break
      (while (listp parts)
        (let ((traverse (funcall func (car parts) state)))
          (setq parts (cdr parts))
          (if parts
              (setq state (cdr traverse))
              (throw 'break (cdr traverse))))))))

(defun dotassoc (expr table)
  "Dotted expression handling with `assoc'."
  (dotassoc-fn expr table 'assoc))

(defun dotassq (expr table)
  "Dotted expression handling with `assq'."
  (dotassoc-fn expr table 'assq))

(ert-deftest dotassoc ()
  (should
   (equal
    (dotassoc "a.b.c" '(("a" . (("b" . (("c" . 10)))))))
    10)))

(ert-deftest dotassq ()
  (should
   (equal
    (dotassq 'a.b.c '((a . ((b . ((c . 10)))))))
    10)))

(provide 'dotassoc)

;;; dotassoc.el ends here
