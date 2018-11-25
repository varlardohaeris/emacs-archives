;;; compilation-emacs21-batch.el --- error regexps for emacs21 byte compiler

;; Copyright 2008, 2009, 2010, 2015 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 6
;; Keywords: processes, compilation
;; URL: http://user42.tuxfamily.org/compilation-emacs21-batch/index.html
;; EmacsWiki: CompilationMode

;; compilation-emacs21-batch.el is free software; you can redistribute it
;; and/or modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; compilation-emacs21-batch.el is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This spot of code adds a pattern to `compilation-error-regexp-alist' for
;; the old style Emacs 21 and earlier (including XEmacs 21) batch byte
;; compiling error messages like
;;
;;     While compiling foo-mode in file /my/dir/foo.el:
;;       !! assignment to free variable foo-bar-quux
;;
;; There's only a filename in the message, no line number, so it's not
;; terrifically helpful, but at least in a compilation buffer the messages
;; are highlighted and go to the offending file.
;;
;; If you only ever `M-x byte-compile-file' etc interactively from within
;; Emacs then you won't need this.  It's geared to running
;;
;;     emacs -batch -f batch-byte-compile
;;
;; from a makefile or script.
;;
;; And Emacs 22 and up prints the usual GNU "file:line:column:" style, so
;; needs no special help.

;;; Install:

;; Put compilation-emacs21-batch.el in one of your `load-path' directories,
;; and in your .emacs add
;;
;;     (eval-after-load "compile" '(require 'compilation-emacs21-batch))
;;
;; There's an autoload cookie for this if you install by
;; `M-x package-install' or know `update-file-autoloads' and friends.

;;; Emacsen:

;; Designed for Emacs 22, works in Emacs 20 and 21 and XEmacs 21 too.

;;; History:

;; Version 1 - the first version
;; Version 2 - tweak comments
;; Version 3 - new home page
;; Version 4 - unload-feature support
;; Version 5 - new email
;; Version 6 - quieten emacs20 byte compiler a bit

;;; Code:

;;;###autoload (eval-after-load "compile" '(require 'compilation-emacs21-batch))

(require 'compile)

;; quieten the emacs20 byte compiler by showing it a `remove' function,
;; though that function is not actually called there
(eval-when-compile
  (unless (fboundp 'remove)
    (require 'cl))) ;; for `remove' in emacs20

;; variable in xemacs21, quieten emacs20 byte compiler
(eval-when-compile
  (unless (boundp 'compilation-error-regexp-alist-alist)
    (defvar compilation-error-regexp-alist-alist)))


;; "!!" is a fatal error and "**" is a warning but in practice you have to
;; check manually to see if warnings like "not known to be defined" are in
;; fact errors in the code.  So don't distinguish the two in the emacs22
;; error/warning matching system.
;;
;; Subexp 2 for the linenum is a dummy.  There's no line number but emacs21
;; and xemacs21 demand a subexp number there.  The text matched by subexp 2
;; is not a number, and becomes 0 from string-to-int, which is enough.
;; (emacs21 and up allows a function to call to get a linenum, which could
;; return 1, but xemacs21 doesn't have that.)
;;
;; See emacs21 `byte-compile-log-1' for the "While ... in file ..." message
;; print.
;;
(let ((symbol 'compilation-emacs21-batch)
      (pattern '("^While compiling .* in file \\(.*\\):\n  \\(!!\\|\\*\\*\\) "
                 1 2)))

  (cond ((eval-when-compile (boundp 'compilation-error-regexp-systems-list))
         ;; xemacs21
         (add-to-list 'compilation-error-regexp-alist-alist
                      (list symbol pattern))
         (compilation-build-compilation-error-regexp-alist))

        ((eval-when-compile (boundp 'compilation-error-regexp-alist-alist))
         ;; emacs22
         (add-to-list 'compilation-error-regexp-alist symbol)
         (add-to-list 'compilation-error-regexp-alist-alist
                      (cons symbol pattern)))

        (t
         ;; emacs21
         (add-to-list 'compilation-error-regexp-alist pattern))))

(defun compilation-emacs21-batch-unload-function ()
  "Remove compilation-emacs21-batch regexps.
This is called by `unload-feature'."
  ;; in emacs20 `remove' is in cl.el, but that's ok since this unload isn't
  ;; called there
  (setq compilation-error-regexp-alist
        (remove 'compilation-emacs21-batch compilation-error-regexp-alist))
  (setq compilation-error-regexp-alist-alist
        (remove (assq 'compilation-emacs21-batch
                      compilation-error-regexp-alist-alist)
                compilation-error-regexp-alist-alist))
  (when (eval-when-compile
          (fboundp 'compilation-build-compilation-error-regexp-alist))
    (compilation-build-compilation-error-regexp-alist))
  nil) ;; and normal unload-feature actions

;; LocalWords: quux regexps dir filename makefile foo

(provide 'compilation-emacs21-batch)

;;; compilation-emacs21-batch.el ends here
