;;; perl-use-utf8-coding.el --- coding system from "use utf8" in perl code

;; Copyright 2009, 2010, 2015 Kevin Ryde
;;
;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 5
;; Keywords: i18n, perl
;; URL: http://user42.tuxfamily.org/perl-use-utf8-coding/index.html
;; EmacsWiki: PerlLanguage

;; perl-use-utf8-coding.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; perl-use-utf8-coding.el is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This spot of code gets an Emacs coding system from a "use utf8;" pragma
;; in Perl code.  That pragma means strings and identifiers have or may have
;; utf-8 encoded wide chars.  See the `perl-use-utf8-auto-coding-function'
;; docstring below for more.

;;; Install:

;; Put perl-use-utf8-coding.el in one of your `load-path' directories, and in
;; your .emacs add
;;
;;     (require 'perl-use-utf8-coding)
;;
;; There's an autoload cookie for this, if you use `update-file-autoloads'
;; and friends.

;;; Emacsen:

;; Designed for Emacs 21 and up.  Does nothing in XEmacs 21.

;;; History:

;; Version 1 - the first version
;; Version 2 - fix defadvice ad-return-value test
;; Version 3 - undo defadvice on unload-feature
;; Version 4 - allow for advice unloaded before us too
;; Version 5 - new email

;;; Code:

;; This autoload and the recommended install above is simply a `require'
;; because it's not worth autoloading functions in `auto-coding-functions'.
;; Those functions are called almost immediately in a normal Emacs startup,
;; eg. when reading any .el files under "site-start", so an autoloaded func
;; doesn't defer anything.  Likewise for the `set-auto-coding' defadvice way
;; for emacs21.
;;
;;;###autoload (require 'perl-use-utf8-coding)

(defun perl-use-utf8-auto-coding-function (size)
  "Return a coding system from a \"use utf8;\" in Perl code.
This function is designed for use in `auto-coding-functions'.

At point there should be SIZE many bytes from an
`insert-file-contents-literally' of a file or part of a file.
If there's a

    use utf8;

pragma and coding system `utf-8' is known in this Emacs then the
return is symbol `utf-8'.  Otherwise the return is nil.

\"use utf8;\" must be at the start of the line, unindented, and
on its own.  A comment is allowed like

    use utf8;   # for various strings

There's no limit how far into the buffer the pragma might be, in
case there's a big block of POD documentation first.

Requiring the start of a line avoids indented sample code or
ordinary text discussing utf8 in the middle of a paragraph.
Unindented Perl code within a shell script or similar may be
matched, but that's usually good if the pragma really indicate
there's utf8 bytes following.

The most likely false match is sample code in TeX, Texinfo, nroff
or similar formats with directives for indentation rather than
actual spaces or tabs.  Hopefully such files are either utf-8
themselves anyway, or ASCII-only, so a return `utf-8' is ok.

------
It's easy to write code which does \"use utf8;\" in a wildly
obscured way.   Only normal basic use is noticed by
perl-use-utf8-coding.el.

Emacs 22 and up can often detect utf-8 itself just from the
bytes, but looking for \"use utf8;\" makes sure.

Perl code might also have utf-8 as raw bytes in strings instead
of wide chars.  Such code won't have \"use utf8;\" and you should
instead use one of the other Emacs coding system
mechanisms (cookie, `file-coding-system-alist', etc) to edit with
the chars decoded.

The perl-use-utf8-coding.el home page is
URL `http://user42.tuxfamily.org/perl-use-utf8-coding/index.html'"

  ;; emacs21 up has utf-8 built-in always and could probably have this
  ;; coding-system-p locked down with an eval-when-compile.  Only in
  ;; xemacs21 does support vary, and there's no setups below for xemacs.
  ;;
  (and (coding-system-p 'utf-8)

       (save-excursion
         (let ((case-fold-search nil))
           (and (re-search-forward
                 "^use utf8;\\s-*\\([#\r\n]\\|\\'\\)"
                 nil (+ (point) size))
                'utf-8)))))

(cond ((eval-when-compile (boundp 'auto-coding-functions))
       ;; emacs 22 up
       (add-to-list 'auto-coding-functions 'perl-use-utf8-auto-coding-function
                    t) ;; append

       ;; Note no
       ;;
       ;;     (custom-add-option 'auto-coding-functions
       ;;                        'perl-use-utf8-auto-coding-function)
       ;;
       ;; used here, since as of emacs 23 `auto-coding-functions' only has
       ;; custom type "(repeat function)" and it doesn't take possible
       ;; values like that.  If you do it `customize-variable' throws an
       ;; error.
       ;;
       )

      ((eval-when-compile (fboundp 'set-auto-coding))
       ;; emacs 21
       ;;
       ;; `set-auto-coding' moves point, when successful at least.  Don't
       ;; save-excursion around ad-do-it, just in case something depends on
       ;; point moving, but do save the position to look from.
       ;;
       (defadvice set-auto-coding (around perl-use-utf8-coding activate)
         "Find the coding system from a \"use utf8\" pragma in Perl code.
See `perl-use-utf8-auto-coding-function' for details."

         (let ((perl-use-utf8-coding--saved-point (point)))
           ad-do-it
           (unless ad-return-value
             (save-excursion
               (goto-char perl-use-utf8-coding--saved-point)
               (setq ad-return-value (perl-use-utf8-auto-coding-function size))))))
       (add-hook
        'perl-use-utf8-coding-unload-hook
        (lambda ()
          ;; ad-find-advice not autoloaded, so require 'advice in case it
          ;; was removed by `unload-feature'
          (require 'advice)
          (when (ad-find-advice 'set-auto-coding 'around 'perl-use-utf8-coding)
            (ad-remove-advice   'set-auto-coding 'around 'perl-use-utf8-coding)
            (ad-activate        'set-auto-coding)))))

      ;; what does xemacs have?
      )

;; LocalWords: utf pragma docstring unindented Texinfo nroff ok el

(provide 'perl-use-utf8-coding)

;;; perl-use-utf8-coding.el ends here
