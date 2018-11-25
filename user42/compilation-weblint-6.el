;;; compilation-weblint.el --- error regexps for weblint

;; Copyright 2007, 2008, 2009, 2010 Kevin Ryde

;; Author: Kevin Ryde <user42@zip.com.au>
;; Version: 6
;; Keywords: processes
;; URL: http://user42.tuxfamily.org/compilation-weblint/index.html
;; EmacsWiki: CompilationMode

;; compilation-weblint.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; compilation-weblint.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This spot of code adds a `compilation-error-regexp-alist' pattern for
;; messages from the weblint program, the one based on the Perl HTML::Lint,
;;
;;     http://search.cpan.org/dist/HTML-Lint/
;;
;; Emacs 23 has incorporated this feature, so the code does nothing there.

;;; Install:

;; Put compilation-weblint.el in one of your `load-path' directories, and in
;; your .emacs add
;;
;;     (eval-after-load "compile" '(require 'compilation-weblint))
;;
;; There's an autoload cookie below for this, if you know how to use
;; `update-file-autoloads' and friends.

;;; Emacsen:

;; Designed for Emacs 21 and 22.  Works in Emacs 20 and XEmacs 21.
;; Not needed and does nothing in Emacs 23.

;;; History:

;; Version 1 - the first version
;; Version 2 - put the eval-after-load in .emacs
;; Version 3 - notice emacs23 now has this itself
;; Version 4 - xemacs eval-after-load only takes a string
;; Version 5 - disable 4.3BSD lint pass 3 on xemacs too, defang and tag
;;             instead of removing outright
;; Version 6 - correction to bsd lint disabling in xemacs21


;;; Code:

;;;###autoload (eval-after-load "compile" '(require 'compilation-weblint))

(require 'compile)

;; The messages come from HTML::Lint::Error::as_string(), eg.
;;
;;     index.html (13:1) Unknown element <fdjsk>
;;
;; The pattern only matches filenames without spaces, since that should
;; be usual and should help reduce the chance of a false match of a
;; message from some unrelated program.
;;
;; This message style is quite close to the "ibm" entry of emacs22
;; `compilation-error-regexp-alist-alist' which is for IBM C, but that ibm
;; one doesn't have a space after the filename.
;;
(let ((symbol  'compilation-weblint)
      (pattern '("^\\([^ \t\r\n(]+\\) (\\([0-9]+\\):\\([0-9]+\\)) "
		 1 2 3)))

  (cond ((eval-when-compile (boundp 'compilation-error-regexp-systems-list))
         ;; xemacs21
         (add-to-list 'compilation-error-regexp-alist-alist
                      (list symbol pattern)))

        ((eval-when-compile
           (and (eval-when-compile ;; quieten emacs21 byte compiler
                  (boundp 'compilation-error-regexp-alist-alist))
                (assq 'weblint compilation-error-regexp-alist-alist)))
         ;; emacs23 -- `weblint' already present, do nothing
         )

        ((eval-when-compile (boundp 'compilation-error-regexp-alist-alist))
         ;; emacs22
         (add-to-list 'compilation-error-regexp-alist
                      symbol)
         (add-to-list 'compilation-error-regexp-alist-alist
                      (cons symbol pattern)))

        (t
         ;; emacs21
         (add-to-list 'compilation-error-regexp-alist pattern))))

;; Remove the "4.3BSD lint pass 3" element from emacs21 and xemacs21 because
;; it wrongly matches weblint messages.  It's apparently supposed to match
;; something like
;;
;;     bloofle defined( /users/wolfgang/foo.c(4) ) ...
;;
;; but it's rather loose and on a weblint line like
;;
;;     index.html (13:1) Unknown element <fdjsk>
;;
;; it matches "(13:1)" as if "13" is the filename and "1" is the line
;; number.  Disabling it is a bit nasty, but emacs22 has dropped it, so
;; consider this an upgrade!
;;
(let* ((regexp   ".*([ \t]*\\([a-zA-Z]?:?[^:( \t\n]+\\)[:(][ \t]*\\([0-9]+\\))")
       ;; the "^ ^" cannot match anything :-)
       (disabled (concat "^ ^ compilation-weblint.el disabled \"4.3BSD lint pass 3\" as it mismatches weblint output -- " regexp)))

  ;; emacs21 -- pattern elem like (REGEXP ...)
  (setq compilation-error-regexp-alist
        (mapcar (lambda (elem)
                  (if (equal regexp (car-safe elem))
                      (cons disabled (cdr elem))
                    elem))
                compilation-error-regexp-alist))

  ;; xemacs21 -- elem like (4bsd (REGEXP ...))
  (when (eval-when-compile
          (fboundp 'compilation-build-compilation-error-regexp-alist))
    (setq compilation-error-regexp-alist-alist
          (mapcar (lambda (elem)
                    (when (equal regexp (car-safe (car-safe (cdr-safe elem))))
                      (setq elem (copy-tree elem))
                      (setcar (car-safe (cdr-safe elem)) disabled))
                    elem)
                  compilation-error-regexp-alist-alist))))

(when (eval-when-compile
        (fboundp 'compilation-build-compilation-error-regexp-alist))
  ;; xemacs, for the weblint addition and possible bsd removal
  (compilation-build-compilation-error-regexp-alist))

(provide 'compilation-weblint)

;;; compilation-weblint.el ends here
