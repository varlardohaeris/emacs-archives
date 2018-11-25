;;; compilation-ledger.el --- error regexps for ledger

;; Copyright 2009, 2010, 2011, 2015 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 3
;; Keywords: processes, compilation
;; URL: http://user42.tuxfamily.org/compilation-ledger/index.html
;; EmacsWiki: CompilationMode

;; compilation-ledger.el is free software; you can redistribute it
;; and/or modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; compilation-ledger.el is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This spot of code adds a `compilation-error-regexp-alist' pattern to
;; recognise error messages from the "ledger" program,
;;
;;     http://newartisans.com/software/ledger.html
;;
;; such as
;;
;;     Error: "foo.ledger", line 1656: Invalid date string: foo
;;
;; This is only for running ledger from M-x compile.  (ledger.el gives
;; reports with its own `ledger-report-mode' which has more features and
;; isn't based on `compilation-mode'.)

;;; Install:

;; Put compilation-ledger.el in one of your `load-path' directories,
;; and in your .emacs add
;;
;;     (eval-after-load "compile" '(require 'compilation-ledger))
;;
;; There's an autoload cookie for this if you install by
;; `M-x package-install' or know `update-file-autoloads' and friends.

;;; Emacsen:

;; Designed for Emacs 20 up, works in XEmacs 21 too.

;;; History:

;; Version 1 - the first version
;; Version 2 - new email
;; Version 3 - quieten emacs20 byte compile a bit

;;; Code:

;;;###autoload (eval-after-load "compile" '(require 'compilation-ledger))

(require 'compile)

;; variable in xemacs21, quieten emacs20 byte compiler
(eval-when-compile
  (unless (boundp 'compilation-error-regexp-alist-alist)
    (defvar compilation-error-regexp-alist-alist)))


(let ((symbol  'compilation-ledger)
      (pattern '("^Error: \"\\([^\"\n]+?\\)\", line \\([0-9]+\\):" 1 2)))
  (cond ((eval-when-compile (boundp 'compilation-error-regexp-systems-list))
         ;; xemacs21
         (add-to-list 'compilation-error-regexp-alist-alist
                      (list symbol pattern))
         (compilation-build-compilation-error-regexp-alist))
        ((eval-when-compile (boundp 'compilation-error-regexp-alist-alist))
         ;; emacs22 up
         (add-to-list 'compilation-error-regexp-alist symbol)
         (add-to-list 'compilation-error-regexp-alist-alist
                      (cons symbol pattern)))
        (t
         ;; emacs21
         (add-to-list 'compilation-error-regexp-alist pattern))))

(defun compilation-ledger-unload-function ()
  "Remove compilation-ledger regexps.
This is called by `unload-feature'."

  ;; emacs22 up
  (setq compilation-error-regexp-alist
        (remove 'compilation-ledger compilation-error-regexp-alist))
  (setq compilation-error-regexp-alist-alist
        (remove (assq 'compilation-ledger
                      compilation-error-regexp-alist-alist)
                compilation-error-regexp-alist-alist))

  ;; xemacs21
  (when (eval-when-compile
          (fboundp 'compilation-build-compilation-error-regexp-alist))
    (compilation-build-compilation-error-regexp-alist))

  nil) ;; and normal unload-feature actions

;; LocalWords: http newartisans com html el foo regexps

(provide 'compilation-ledger)

;;; compilation-ledger.el ends here
