;;; perl-env-substitute.el --- Perl style $ENV{VARNAME} in substitute-in-filename

;; Copyright 2010, 2015 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 3
;; Keywords: convenience, perl
;; URL: http://user42.tuxfamily.org/perl-env-substitute/index.html

;; perl-env-substitute.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; perl-env-substitute.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `M-x perl-env-substitute-enable' extends `substitute-in-file-name' to
;; expand Perl style $ENV{VARNAME} as well as shell $VARNAME or ${VARNAME}.
;;
;; The main use is for following an interpolated string with M-x ffap.  By
;; default ffap doesn't accept braces {} at all, but
;; `perl-env-substitute-ffap-braces' can set that up, which is good for
;; shell script code with ${HOME} etc too.

;;; Install:

;; Put perl-env-substitute.el in one of your `load-path' directories and to
;; make the functions available add to in your .emacs
;;
;;     (autoload 'perl-env-substitute-enable      "perl-env-substitute" nil t)
;;     (autoload 'perl-env-substitute-ffap-braces "perl-env-substitute" nil t)
;;
;; You can enable it for ffap use with something like
;; 
;;     (eval-after-load "ffap"
;;       '(progn
;;          (perl-env-substitute-enable)
;;          (perl-env-substitute-ffap-braces)))
;;
;; The functions are `interactive' so M-x perl-env-substitute-enable etc can
;; try them or use them just sometimes.
;;
;; There's autoload cookies for the functions if you install via
;; `M-x package-install' or know `update-file-autoloads'.

;;; History:
;;
;; Version 1 - the first version
;; Version 2 - new email
;; Version 3 - cl for dolist in emacs20

;;; Code:

;; Explicit dependency on advice.el since `perl-env-substitute-unload-function'
;; needs `ad-find-advice' macro when running not byte compiled, and that
;; macro is not autoloaded.
(require 'advice)

(eval-when-compile
  (unless (fboundp 'dolist)
    (require 'cl))) ;; for `dolist' macro in emacs20

;;-----------------------------------------------------------------------------

(defconst perl-env-substitute-regexp
  "\\$\\(ENV\\){\
\\([^'\"}]*\
\\|\\('\\)[^'\"}]*\\('\\)\
\\)\\(}\\|\\'\\)"
  "Regexp for $ENV{VARNAME} or $ENV{'VARNAME'}.
In its current form group 1 is ENV, groups 3 and 4 are the '
quotes, if present.")

(defun perl-env-substitute-ENV (str)
  "Turn Perl style $ENV{VARNAME} forms in STR into ${VARNAME}.
Quotes $ENV{'VARNAME'} or or $ENV{\"VARNAME\"} are stripped too."
  (if (string-match perl-env-substitute-regexp str)
      (with-temp-buffer
        (insert str)
        (goto-char (point-min))
        (let ((case-fold-search nil))
          (while (re-search-forward perl-env-substitute-regexp nil t)
            (when (match-beginning 3)
              ;; quotes around 'FOO'
              (delete-region (match-beginning 4) (match-end 4))
              (delete-region (match-beginning 3) (match-end 3)))
            ;; ENV bit
            (delete-region (match-beginning 1) (match-end 1))))
        (buffer-string))
    str))

(defadvice substitute-in-file-name (before perl-env-substitute)
  "Substitute Perl style $ENV{VARNAME} forms too."
  (if (fboundp 'perl-env-substitute-ENV) ;; in case unloaded
      (setq filename (perl-env-substitute-ENV filename))))

;;;###autoload
(defun perl-env-substitute-enable ()
  "Setup Perl style $ENV{VARNAME} in `substitute-in-file-name'."
  (interactive)
  (ad-enable-advice 'substitute-in-file-name 'before 'perl-env-substitute)
  (ad-activate      'substitute-in-file-name))

;; `-unload-function' only runs in emacs22 up, so the defadvice is defanged
;; for when the rest is unloaded in emacs21 and xemacs21.  Removing the
;; advice is good as a cleanup though.
;;
(defun perl-env-substitute-unload-function ()
  "Remove defadvice from `substitute-in-file-name'.
This is called by `unload-feature'."
  (when (ad-find-advice 'substitute-in-file-name 'before 'perl-env-substitute)
    (ad-remove-advice   'substitute-in-file-name 'before 'perl-env-substitute)
    (ad-activate        'substitute-in-file-name))
  nil)

;;-----------------------------------------------------------------------------

(defvar ffap-string-at-point-mode-alist)

;;;###autoload
(defun perl-env-substitute-ffap-braces ()
  "Add braces {} to `file' chars in `ffap-string-at-point-mode-alist'."
  (interactive)
  (require 'ffap)
  (setq ffap-string-at-point-mode-alist
        (mapcar (lambda (elem)
                  (when (eq 'file (car-safe elem))
                    (dolist (brace '("{" "}"))
                      (unless (string-match brace (cadr elem))
                        (setq elem (cons 'file
                                         (cons (concat (cadr elem) brace)
                                               (cddr elem)))))))
                  elem)
                ffap-string-at-point-mode-alist)))

(provide 'perl-env-substitute)

;;; perl-env-substitute.el ends here
