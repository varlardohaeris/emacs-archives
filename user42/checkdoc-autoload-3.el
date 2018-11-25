;;; checkdoc-autoload.el --- check for various autoload cookies

;; Copyright 2009, 2010, 2015 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 3
;; Keywords: lisp, maint, checkdoc
;; URL: http://user42.tuxfamily.org/checkdoc-autoload/index.html

;; checkdoc-autoload.el is free software; you can redistribute
;; it and/or modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; checkdoc-autoload.el is distributed in the hope that it will
;; be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This spot of code checks ;;;###autoload cookies in Lisp sources on
;;
;;     - an interactive command as an entrypoint
;;     - defgroup forms
;;     - `put risky-local-variable' and similar
;;
;; See the docstrings below of `checkdoc-autoload-entrypoint',
;; `checkdoc-autoload-defgroup' and `checkdoc-autoload-puts' for details.

;;; Emacsen:

;; Designed for Emacs 21 and up, works in XEmacs 21 too.

;;; Install:

;; Put checkdoc-autoload.el in one of your `load-path' directories,
;; and in your .emacs add
;;
;;     (autoload 'checkdoc-autoload-comment "checkdoc-autoload")
;;     (add-hook 'checkdoc-comment-style-hooks 'checkdoc-autoload-comment)
;;
;; Or the checkdoc-autoload-defgroup, checkdoc-autoload-puts, and
;; checkdoc-autoload-entrypoint parts separately if desired.
;;
;; There's autoload cookies for the functions, if you know how to use
;; `update-file-autoloads' and friends, then add to or customize
;; `checkdoc-comment-style-hooks'.

;;; History:

;; Version 1 - the first version
;; Version 2 - new checkdoc-autoload-defgroup and checkdoc-autoload-puts
;;           - separate checkdoc-autoload-entrypoint
;; Version 3 - new email

;;; Code:

(require 'checkdoc)
(require 'autoload) ;; for `generate-autoload-cookie'


;;-----------------------------------------------------------------------------
;; generic

(defun checkdoc-autoload-forward-comments ()
  "Move point forward across comments and whitespace."

  ;; xemacs 21.4.22 `forward-comment' is a bit buggy at end of buffer.  It
  ;; loops for the full specified count, without noticing end of buffer.
  ;; Using "point-max - point" is enough to cover all possible comments and
  ;; avoids a huge count in a big buffer.  The slowdown as such seems
  ;; noticable only at about 10,000,000 anyway.  A loop `(while
  ;; (forward-comment 1))' is no good, since in that xemacs the return is t
  ;; at end of buffer even when no comment has been skipped or point moved
  ;; at all.
  (forward-comment (- (point-max) (point))))

(defun checkdoc-autoload-quoted-symbol (form)
  "If FORM is a list (quote foo) return symbol foo, otherwise return nil."
  (and (eq 'quote (car-safe form))
       (symbolp (car-safe (cdr form)))
       (null (cddr form))
       (cadr form)))
(eval-when-compile
  (put 'checkdoc-autoload-quoted-symbol 'side-effect-free 'error-free))
(eval-when-compile
  (put 'checkdoc-autoload-quoted-symbol 'pure t))

;;-----------------------------------------------------------------------------
;; shared

(defun checkdoc-autoload-loop (func)
  "Call (FUNC obj pos cookie) for each object or form in the current buffer.
This is an internal part of checkdoc-autoload.el.

POS is the position of the start of OBJ.  COOKIE is non-nil if
OBJ is preceded by an ;;;###autoload cookie."

  (let ((cookie-regexp (concat "^" (regexp-quote generate-autoload-cookie)
                               "\n")))
    (save-excursion
      (goto-char (point-min))
      (catch 'return
        (while t
          (checkdoc-autoload-forward-comments)
          (let* ((pos    (point))
                 (cookie (save-excursion
                           (forward-line -1)
                           (looking-at cookie-regexp)))
                 (obj    (condition-case err
                             (read (current-buffer))
                           (end-of-file
                            (throw 'return nil)) ;; all ok
                           (error
                            (throw 'return
                                   (checkdoc-create-error
                                    (format "Cannot parse lisp form: %s"
                                            (error-message-string err))
                                    (point)
                                    (line-end-position)
                                    t)))))) ;; unfixable
            (funcall func obj pos cookie)))))))


;;-----------------------------------------------------------------------------
;; checkdoc-autoload-defgroup

;;;###autoload
(defun checkdoc-autoload-defgroup ()
  "Check that `defgroup' forms are autoloaded.
This function is designed for use in `checkdoc-comment-style-hooks'.

A `defgroup' can be helpfully autoloaded in Emacs 22 up so as to
appear in its parent groups and be known to a direct
`customize-group'.

    ;;;###autoload
    (defgroup some-group ...)

In Emacs 21 and XEmacs 21 an autoloaded defgroup just puts the
full form in the loaddefs.  This doesn't have the desired effect,
but does no harm.  The group appears in its parent, but on
entering it's empty."

  (checkdoc-autoload-loop
   (lambda (obj pos cookie)
     (when (and (not cookie)
                (eq 'defgroup (car-safe obj)))
       (if (checkdoc-y-or-n-p
            (format "`defgroup' can be helpfully autoloaded.  Add cookie? "))
           (save-excursion
             (goto-char pos)
             (insert generate-autoload-cookie "\n"))

         ;; not fixed, stop like builtin checks do
         (throw 'return
                (checkdoc-create-error
                 "`defgroup' should be autoloaded"
                 pos pos)))))))


;;-----------------------------------------------------------------------------
;; checkdoc-autoload-puts

(defun checkdoc-autoload-put-p (form)
  "Return non-nil if FORM is a `put' of local-variable riskiness.
This is an internal part of checkdoc-autoload.el.
FORM can be either

    (put 'something 'risky-local-variable t)
or
    (put 'something 'safe-local-variable 'predicate)

The return is the symbol `risky-local-variable' or
`safe-local-variable' \(just used as a diagnostic currently.

If FORM is anything else, including a non-list or mis-shapen
nonsense, the return is nil."

  (and (eq 'put (car-safe form))
       (checkdoc-autoload-quoted-symbol (car-safe (cdr form)))
       (let ((type (checkdoc-autoload-quoted-symbol (car-safe (cddr form)))))
         (and (memq type '(safe-local-variable
                           risky-local-variable))
              type))))

;;;###autoload
(defun checkdoc-autoload-puts ()
  "Check that \"safe\" and \"risky\" variable properties are autoloaded.
This function is designed for use in `checkdoc-comment-style-hooks'.

If you set the `safe-local-variable' or `risky-local-variable'
property on a variable then that setting should be autoloaded
with an autoload cookie ready for `update-file-autoloads' and
friends to pick up.

    ;;;###autoload
    (put 'some-variable 'safe-local-variable 'integerp)

    ;;;###autoload
    (put 'another-variable 'risky-local-variable t)

Local variable settings (see Info node `File Variables') are made
before loading the code for a major mode, so the safety or
riskiness must be pre-loaded to take effect, otherwise a setting
is \"unsafe\" by default.

Lisp packages pre-loaded in Emacs itself or designed only for an
early `require' in .emacs don't need autoloads but
`checkdoc-autoload-puts' doesn't have anything to tag that, as
yet.

Autoload cookies of course only do any good when using
`update-file-autoloads' and only if the generated autoloads are
loaded before any potentially risky files."

  (checkdoc-autoload-loop
   (lambda (obj pos cookie)
     (unless cookie
       (let ((type (checkdoc-autoload-put-p obj)))
         (when type
           (if (checkdoc-y-or-n-p
                (format "`put %s' should be autoloaded.  Add cookie? "
                        type))
               (save-excursion
                 (goto-char pos)
                 (insert generate-autoload-cookie "\n"))

             ;; not fixed, stop like builtin checks do
             (throw 'return
                    (checkdoc-create-error
                     (format "`put %s' settings should be autoloaded"
                             type)
                     pos pos)))))))))


;;-----------------------------------------------------------------------------
;; checkdoc-autoload-entrypoint

(defun checkdoc-autoload-interactive-defun-p (form)
  "Return non-nil if a defun FORM has an `interactive'.
This is an internal part of checkdoc-autoload.el.
FORM is expected to be a list `(defun ...)'.  The return is nil
if it's some mis-shapen nonsense."
  (setq form (cdr-safe           ;; skip defun
              (cdr-safe          ;; skip func name
              (cdr-safe form)))) ;; skip args
  (if (stringp (car-safe form))  ;; skip possible docstring
      (setq form (cdr-safe form)))
  (setq form (car-safe form))    ;; first form in body
  (eq 'interactive (car-safe form)))

;; Symbol property `checkdoc-autoload-interactive-p' is whether a form
;; (SYMBOL ...) is interactive, ie. a user command.  The property value is
;; nil or t, or a function to be called (FUNC form).
;;
;; `defadvice' can add interactiveness to an external function, but ignore
;; that because it's not a newly defined func in the current file and may or
;; may not want an ;;;###autoload in the current file.
;;
(put 'defun 'checkdoc-autoload-interactive-p
     'checkdoc-autoload-interactive-defun-p)
(put 'define-derived-mode           'checkdoc-autoload-interactive-p t)
(put 'define-generic-mode           'checkdoc-autoload-interactive-p t)
(put 'define-minor-mode             'checkdoc-autoload-interactive-p t)
(put 'define-globalized-minor-mode  'checkdoc-autoload-interactive-p t)
(put 'easy-mmode-define-global-mode 'checkdoc-autoload-interactive-p t)
(put 'easy-mmode-define-minor-mode  'checkdoc-autoload-interactive-p t)

(defun checkdoc-autoload-interactive-p (obj)
  "Return non-nil if OBJ is a candidate to be autoloaded.
This means a list (defun foo () (interactive) ...), or
\(define-minor-mode ...), etc."

  (and (consp obj)
       (symbolp (car obj))
       (let ((prop (get (car obj) 'checkdoc-autoload-interactive-p)))
         (cond ((not prop) nil) ;; unknown or not interactive
               ((eq t prop) t)  ;; yes, interactive
               (t (funcall prop obj)))))) ;; else call

;;;###autoload
(defun checkdoc-autoload-entrypoint ()
  "Check at least one `interactive' command has an autoload.
This function is designed for use in `checkdoc-comment-style-hooks'.

The idea is that if a package has a user-level command then it
should have an autoload cookie on one of them as an entrypoint,
ready for `update-file-autoloads' and friends to pick up.  Even
if you don't use that autoload/loaddefs mechanism yourself it's
good for others who might.

    ;;;###autoload
    (defun foo-mode ()
      (interactive)
      ...

A sub-package or something designed to act just from `require'
might not have anything to autoload.  You can suppress
`checkdoc-autoload-comment' with the following magic comment at
the start of a line somewhere in the file

    ;; checkdoc-autoload: no-entrypoint

Having a package do something when `require'd is usually a bad
idea, because it's then hard to load the file to re-use its
functions for other things.  Better some sort of foo-enable
function, or even a minor mode, probably with an autoload cookie
and perhaps a `custom-add-option' to show the enable in likely
hooks."

  (unless (save-excursion
            (goto-char (point-min))
            (re-search-forward
             "^;+\\s-*checkdoc-autoload: no-entrypoint\\s-*$" nil t))

    (let ((saw-interactive nil))
      (or (checkdoc-autoload-loop
           (lambda (obj pos cookie)
             (when (checkdoc-autoload-interactive-p obj)
               (when cookie
                 ;; autoloaded interactive, so ok
                 (setq saw-interactive nil)
                 (throw 'return nil))
               ;; non-autoloaded interactive
               (setq saw-interactive t))))

          (and saw-interactive
               (checkdoc-create-error
                (format "No `interactive' command has an %s cookie"
                        generate-autoload-cookie)
                (point-max)
                (point-max)
                t)))))) ;; unfixable, for now


;;-----------------------------------------------------------------------------

;;;###autoload
(defun checkdoc-autoload-comment ()
  "Check all the checkdoc-autoload comment things.
This function is designed for use in `checkdoc-comment-style-hooks'.
The checks are

* `checkdoc-autoload-entrypoint' -- an interactive command entrypoint
* `checkdoc-autoload-defgroup' -- autoloading defgroups
* `checkdoc-autoload-puts' -- autoloading risky-local-variable etc"

  (let ((ret (checkdoc-autoload-entrypoint)))
    (let ((err (checkdoc-autoload-defgroup)))
      (setq ret (or ret err)))
    (let ((err (checkdoc-autoload-puts)))
      (setq ret (or ret err)))
    ret))

;; As of emacs23 checkdoc-comment-style-hooks is only a defvar, so
;; custom-add-option does nothing.
;;
;; ;;;###autoload
;; (custom-add-option 'checkdoc-comment-style-hooks 'checkdoc-autoload-defgroup)
;; ;;;###autoload
;; (custom-add-option 'checkdoc-comment-style-hooks 'checkdoc-autoload-comment)
;; ;;;###autoload
;; (custom-add-option 'checkdoc-comment-style-hooks 'checkdoc-autoload-puts)
;; ;;;###autoload
;; (custom-add-option 'checkdoc-comment-style-hooks 'checkdoc-autoload-entrypoint)

;; LocalWords: defgroups entrypoint obj pos loaddefs shapen autoload
;; LocalWords: autoloads autoloaded autoloading docstrings whitespace el foo

(provide 'checkdoc-autoload)

;;; checkdoc-autoload.el ends here
