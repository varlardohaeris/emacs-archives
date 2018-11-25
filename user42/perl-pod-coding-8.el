;;; perl-pod-coding.el --- coding system from =encoding in perl files

;; Copyright 2007, 2008, 2009, 2010, 2015 Kevin Ryde
;;
;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 8
;; Keywords: i18n, perl
;; URL: http://user42.tuxfamily.org/perl-pod-coding/index.html
;; EmacsWiki: PerlPodCoding

;; perl-pod-coding.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; perl-pod-coding.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This code gets an Emacs coding system from the "=encoding" line in Perl
;; POD documentation, either inline in Perl code or a separate pod file.
;;
;; POD files can use utf-8 or utf-16 byte order marker (BOM) sequences and
;; Emacs will recognise those by itself.  But a coding system only by
;; =encoding will generally need some help.

;;; Install:

;; Put perl-pod-coding.el in one of your `load-path' directories, and in
;; your .emacs add
;;
;;     (require 'perl-pod-coding)
;;
;; There's an autoload cookie for this below, if you know how to use
;; `update-file-autoloads' and friends.

;;; Emacsen:

;; Designed for Emacs 21 and 22.  Does nothing in XEmacs 21.

;;; History:

;; Version 1 - the first version
;; Version 2 - don't restrict to near start of the buffer
;; Version 3 - use lwarn when available
;; Version 4 - must be 'warning as symbol for xemacs21 lwarn
;; Version 5 - fix defadvice ad-return-value test
;; Version 6 - undo defadvice on unload-feature
;; Version 7 - allow for advice unloaded before us too
;; Version 8 - new email

;;; Code:

;; This autoload cookie, and the recommended Install above, is simply a
;; `require' because it's not worth trying to autoload any function in
;; `auto-coding-functions'.  Those functions are called almost immediately
;; in a normal Emacs startup, eg. when reading any .el files under
;; "site-start", so an autoloaded func doesn't defer anything.  Likewise the
;; defadvice of `set-auto-coding' below for emacs21.
;;
;;;###autoload (require 'perl-pod-coding)

(defun perl-pod-coding-function (size)
  "Return the coding system for Perl POD, based on an =encoding line.
This function is designed for use from `auto-coding-functions'.

At point there should be SIZE many bytes from an
`insert-file-contents-literally' of the first part of a file.  If
there's an

    =encoding CHARSET

and its CHARSET is known, then return an Emacs coding system.  If
not then return nil.

The =encoding must be at the start of the line with a blank line
before and after (or at the start or end of the buffer).
Hopefully this is tight enough to avoid false matches.  Text
discussing =encoding, as opposed to using it, will probably be
indented or within a paragraph.

There's no limit how far into the buffer the =encoding might be
located.  This copes with program code at the start and POD and
an =encoding only down after an __END__.

Charset names are turned into Emacs coding systems with
`locale-charset-to-coding-system' in Emacs 22 (and up) or with
the `mm-util' package from Gnus for Emacs 21 (you don't have to
be running Gnus to use that).  The possible names are described
in the Perl Encode::Supported documentation.  They can be Perlish
common names like \"utf8\", MIME registered names, or IANA
registered names."

  ;; For reference, perlpod in perl 5.10.1 notes only a single =encoding is
  ;; allowed.  Dunno if some of the formatters allowed successive
  ;; "=encoding"s to make a change at the point they occurred.  Possibly
  ;; yes, but Emacs doesn't have a notion of multiple encodings in the one
  ;; buffer.

  (save-excursion
    (save-restriction
      (narrow-to-region (point) (+ (point) size))
      (goto-char (point-min))
      (and (let ((case-fold-search nil))
             (re-search-forward "\\(\\`\\|\n\\)=encoding \\(.*\\)\\(\\'\\|\n\\(\\'\\|\n\\)\\)"
                                nil t))
           (let ((charset (match-string 2)))

             (or (and (eval-when-compile
                        (fboundp 'locale-charset-to-coding-system))
                      ;; emacs 22
                      (locale-charset-to-coding-system charset))

                 ;; emacs 21
                 (progn
                   (eval-and-compile ;; quieten the byte compiler
                     (require 'mm-util))
                   (let ((coding (mm-charset-to-coding-system charset)))
                     ;; `mm-charset-to-coding-system' returns `ascii'
                     ;; for ascii or us-ascii, but that's not actually a
                     ;; coding system.  Gnus copes with that in various
                     ;; places (usually treating ascii as meaning no
                     ;; conversion), go undecided here.
                     ;;
                     (if (and (eq coding 'ascii)
                              (not (coding-system-p coding)))
                         (setq coding 'undecided))

                     coding))

                 (progn
                   ;; prefer `display-warning' when available, since a plain
                   ;; `message' tends to be overwritten in many cases
                   (if (eval-when-compile (fboundp 'lwarn)) ;; not in emacs 21
                       (lwarn 'i18n 'warning
                              "Unknown POD charset: %s" charset)
                     (message "Unknown POD charset: %s" charset))
                   nil)))))))

(if (eval-when-compile (boundp 'auto-coding-functions))
    ;; emacs 22
    ;;
    ;; "(custom-add-option 'auto-coding-functions 'perl-pod-coding-function)"
    ;; is not used, since as of emacs 22.1 `auto-coding-functions' only has
    ;; custom type "(repeat function)" and it doesn't take possible values
    ;; like that.  If you do it `customize-variable' throws an error.
    ;;
    (add-to-list 'auto-coding-functions 'perl-pod-coding-function)

  ;; emacs 21
  ;;
  ;; `set-auto-coding' moves point, when successful at least.  Don't
  ;; save-excursion around ad-do-it, just in case something depends on point
  ;; moving, but do save the position to look from.
  ;;
  (defadvice set-auto-coding (around perl-pod-coding activate)
    "Find the coding system for reading a perl pod file, based on =encoding.
See `perl-pod-coding-function' for details."
    (let ((perl-pod-coding--saved-point (point)))
      ad-do-it
      (unless ad-return-value
        (save-excursion
          (goto-char perl-pod-coding--saved-point)
          (setq ad-return-value (perl-pod-coding-function size))))))

  (add-hook 'perl-pod-coding-unload-hook
            (lambda ()
              ;; ad-find-advice not autoloaded, so require 'advice in case
              ;; it was removed by `unload-feature'
              (require 'advice)
              (when (ad-find-advice 'set-auto-coding 'around 'perl-pod-coding)
                (ad-remove-advice   'set-auto-coding 'around 'perl-pod-coding)
                (ad-activate        'set-auto-coding)))))

;; LocalWords: utf Charset Perlish

(provide 'perl-pod-coding)

;;; perl-pod-coding.el ends here
