;;; html-coding.el --- coding system from meta in html files (for Emacs 21)

;; Copyright 2005, 2006, 2007, 2009, 2010, 2011, 2012, 2013, 2015 Kevin Ryde
;;
;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 13
;; Keywords: i18n, html
;; URL: http://user42.tuxfamily.org/html-coding/index.html
;; EmacsWiki: HtmlCoding

;; html-coding.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; html-coding.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This is a spot of code for Emacs 21 to get the coding system from a HTML
;; <meta> tag when visiting a .html, .shtml or .htm file.  Emacs 22 has this
;; feature already (in sgml-html-meta-auto-coding-function).  html-coding.el
;; notices that, and does nothing there.
;;
;; mm-util.el is used to map a mime charset name in the html to an emacs
;; coding system (mm-util.el is from Gnus, but you don't need to run Gnus).
;; "windows-NNNN" charsets are recognised too, and load the necessary
;; codepage, when available.

;;; Install:

;; Put html-coding.el in one of your `load-path' directories, and in your
;; .emacs add
;;
;;     (unless (fboundp 'sgml-html-meta-auto-coding-function)
;;       (require 'html-coding))
;;
;; There's an autoload cookie for this below, if you use
;; update-file-autoloads and friends.

;;; History:

;; Version 1 - the first version.
;; Version 2 - tweak comments.
;; Version 3 - add codepage-setup for windows-NNNN, add autoload install
;;             based on suggestion from Jakub Narebski.
;; Version 4 - redo as defadvice on `set-auto-coding', for correct operation
;;             from `archive-mode'.
;; Version 5 - cope with explicit charset=US-ASCII.
;; Version 6 - allow capitals irrespective of case-fold-search setting
;; Version 7 - suggest fboundp test in .emacs, don't use `display-warning'
;; Version 8 - eval-when-compile in the cookie, helping bytecomp'ed loaddefs
;; Version 9 - fix defadvice ad-return-value test
;; Version 10 - undo defadvice on unload-feature
;; Version 11 - allow for advice unloaded before us too
;; Version 12 - use `ignore-errors'
;; Version 13 - cl macros only when needed

;;; Code:

;;;###autoload (unless (eval-when-compile (fboundp 'sgml-html-meta-auto-coding-function)) (require 'html-coding))

(eval-when-compile
  (unless (fboundp 'ignore-errors)
    (require 'cl))) ;; for `ignore-errors'

;; emacs 22 `sgml-html-meta-auto-coding-function' does this coding system
;; determination already, do nothing in that case
;;
(unless (eval-when-compile (fboundp 'sgml-html-meta-auto-coding-function))

  (defun html-auto-coding-function (size)
    "Return the coding system for a html file, based on a <meta> tag.
At point there should be SIZE many bytes from an
`insert-file-contents-literally' of the first part of a file.  If
there's a <meta charset=...> near the start, and the charset is
known, then an Emacs coding system is returned.  If not the
return is nil.

HTML mime charset names are mapped to emacs coding systems using
`mm-util' (from Gnus), plus \"windows-NNNN\" charsets are
explicitly handled though `codepage-setup' (recent versions of
mm-util can do that itself actually).

If you have a file with a slightly bogus charset name, like
\"iso8859-1\" instead of \"iso-8859-1\", then you can map it to
the right one by adding to `mm-charset-synonym-alist'.  (But note
that the mm-util.el which comes with Emacs 21.4a has a bug that
stops `mm-charset-synonym-alist' from working.  The test
`(mm-coding-system-p charset)' should be `(mm-coding-system-p cs)',
ie. validate the mapped good name, not the bad one.  You can make that
change, or it's fixed in the separately packaged Gnus.

The html-coding.el home page is
URL `http://user42.tuxfamily.org/html-coding/index.html'"

    (save-excursion
      (save-restriction
        ;; look only at SIZE being inserted, and only at first 100 lines
        (narrow-to-region (point) (+ (point) size))
        (forward-line 100)
        (narrow-to-region (point-min) (point))
        (goto-char (point-min))

        (and (let ((case-fold-search t))
               (re-search-forward "<meta\\s-[^>]*charset=\\([^\">]+\\)"
                                  (point-max) t))
             (let ((charset (match-string 1)))

               ;; mm-charset-to-coding-system as of Oct 2005 knows how to
               ;; run codepage-setup itself, but do it here so we can work
               ;; on gnus versions prior to that (like plain emacs 21.4)
               ;;
               (or (and (let ((case-fold-search t))
                          (string-match "\\`windows-\\([0-9]+\\)\\'" charset))
                        (ignore-errors
                          (codepage-setup (match-string 1 charset))))

                   (progn
                     (eval-and-compile ;; quieten the byte compiler
                       (require 'mm-util))
                     (let ((coding (mm-charset-to-coding-system charset)))

                       ;; `mm-charset-to-coding-system' returns `ascii'
                       ;; for ascii or us-ascii, but that's not actually a
                       ;; coding system.  Gnus copes with that in various
                       ;; places (usually treating ascii as meaning no
                       ;; conversion), go undecided here.
                       (if (and (eq coding 'ascii)
                                (not (coding-system-p coding)))
                           (setq coding 'undecided))

                       coding))

                   (progn
                     ;; note no `display-warning' in emacs21, however
                     ;; unfortunately a plain `message' tends to be
                     ;; overwritten in many cases :-(
                     (message "Unrecognised HTML MIME charset: %s" charset)
                     nil)))))))

  (defadvice set-auto-coding (around html-coding activate)
    "Find the coding system for reading a HTML file, based on the <meta> tag.
See `html-auto-coding-function' for details."

    ;; `set-auto-coding' moves point, when successful at least.  Don't
    ;; save-excursion around ad-do-it, just in case something depends on
    ;; point moving, but do save the position to look from.
    ;;
    (let ((html-coding--saved-point (point)))
      ad-do-it
      (unless ad-return-value
        (when (let ((case-fold-search t))
                (string-match "\\.\\(html\\|shtml\\|htm\\)\\'" filename))
          (save-excursion
            (goto-char html-coding--saved-point)
            (setq ad-return-value (html-auto-coding-function size)))))))

  (add-hook 'html-coding-unload-hook
            (lambda ()
              ;; ad-find-advice not autoloaded, so require 'advice in case
              ;; it was removed by `unload-feature'
              (require 'advice)
              (when (ad-find-advice 'set-auto-coding 'around 'html-coding)
                (ad-remove-advice   'set-auto-coding 'around 'html-coding)
                (ad-activate        'set-auto-coding)))))

(provide 'html-coding)

;;; html-coding.el ends here
