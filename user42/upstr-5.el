;;; upstr.el --- up-list out of a string or TeX maths too

;; Copyright 2012, 2013, 2014, 2015, 2016 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 5
;; Keywords: convenience
;; URL: http://user42.tuxfamily.org/upstr/index.html

;; upstr.el is free software; you can redistribute
;; it and/or modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; upstr.el is distributed in the hope that it will
;; be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This is a variation of C-M-u `backward-up-list' which will go up out of a
;; string or TeX maths $..$ as well as out of a nested list.
;;
;; This is helpful if you want to move up but point happens to be within a
;; string.  The plain `backward-up-list' doesn't recognise strings and will
;; often error out instead of moving.  `backward-up-list' also doesn't
;; recognise TeX maths $..$, but `forward-sexp' etc do and it's convenient
;; to have them consistent.
;;
;; See the `upstr-up-list-or-string' and `upstr-backward-up-list-or-string'
;; docstrings for details.

;;; Install:
;;
;; Put upstr.el in one of your `load-path' directories and make
;; `M-x upstr-up-list-or-string' and `M-x upstr-backward-up-list-or-string'
;; available by adding to your .emacs
;;
;;     (autoload 'upstr-up-list-or-string          "upstr" nil t)
;;     (autoload 'upstr-backward-up-list-or-string "upstr" nil t)
;;
;; The intention to bind C-M-u to `upstr-backward-up-list-or-string' as a
;; replacement for `backward-up-list'.  C-M-<up> is also `backward-up-list'
;; and can be re-bound similarly.
;;
;;     (global-set-key "\C-\M-u"           'upstr-backward-up-list-or-string)
;;     (global-set-key [(control meta up)] 'upstr-backward-up-list-or-string)
;;
;; If you use C-c } `up-list' in `tex-mode' then it can be re-bound to the
;; upstr equivalent too
;;
;;     (eval-after-load "tex-mode"
;;       '(define-key tex-mode-map "\C-c}" 'upstr-up-list-or-string))
;;
;; There's autoload cookies for the functions if you install via
;; `M-x package-install' or know how to use `update-file-autoloads'.
;; The keybindings are left to personal preference.

;;; Emacsen:
;;
;; Designed for Emacs 20 or higher and XEmacs 21 or higher.

;;; History:

;; Version 1 - the first version
;; Version 2 - docstring on syntax-ppss recalc
;; Version 3 - also TeX maths $...$
;; Version 4 - new email
;; Version 5 - docstring note on unbalanced quotes

;;; Code:

(eval-when-compile
  (unless (fboundp 'ignore-errors)
    (require 'cl))) ;; for `ignore-errors' macro

(eval-when-compile
  (defmacro upstr-interactive-nohat (form)
    "An internal part of upstr.el.
This macro doesn't exist when running byte compiled.
Mangle a defun FORM for `interactive' \"^\" available or not.

FORM is a list (defun ...) with an (interactive \"^p\").  If
\"^\" is not available (new in Emacs 22) then change the form to
plain \"p\"."

    ;; no attempt at generality here, assume there's a docstring and the
    ;; first form is the (interactive "^p")
    ;;
    (unless (condition-case nil ;; test whether ^p works
                (call-interactively (lambda (arg) (interactive "^p") t))
              (error nil))
      (setq form (copy-sequence form)) ;; mangle if it doesn't
      (setcar (nthcdr 4 form) '(interactive "p")))
    form))

(defun upstr-string-start-position ()
  "An internal part of upstr.el.
Return the position of the start of the string containing point,
just before the opening \".  If point is not within a string then
return nil.

           v----point
    \" foo foo foo \"
    ^
    return value"

  (let ((state (if (eval-when-compile (fboundp 'syntax-ppss))
                   (syntax-ppss) ;; emacs22 up
                 ;; emacs21,xemacs21
                 (parse-partial-sexp
                  (save-excursion (beginning-of-defun) (point))
                  (point)))))
    (and (nth 3 state)
         (nth 8 state))))
(eval-when-compile
  (put 'upstr-string-start-position 'side-effect-free t))

(defun upstr-math-start-position (beg)
  "An internal part of upstr.el.
Return the position of the start of a math syntax $ containing
point.  If point is not within a $ pair then return nil.

           v----point
    $$ foo foo foo $$
    ^
    return value

Maths syntax characters in TeX mode are the same $ for open and $
for close so at an arbitrary point position there's no good way
to tell whether inside or out.  The current approach is to search
forward from position BEG to count maths delimiters.  BEG is
presumed to be outside any maths.

The current code doesn't allow for unbalanced $ within string
syntax.  This is a bad problem but shouldn't occur since maths is
only used by `tex-mode' and in that mode there are no strings."

  (let ((limit       (point))
        (limit-close (point))
        ret)
    (save-excursion
      ;; when in the middle of pair $$ the limit-close is before that run of
      ;; $$, so the middle of a closing run is reckoned as inside.
      (when (/= 0 (skip-syntax-forward "$"))
        (skip-syntax-backward "$")
        (setq limit-close (point)))

      (goto-char beg)
      (while (< (point) limit)
        (skip-syntax-forward "^$" limit)  ;; skip outside
        (setq ret (point))
        (if (zerop (skip-syntax-forward "$" limit))  ;; opening $
            (setq ret nil)  ;; no opening $, we're outside

          (skip-syntax-forward "^$" limit)  ;; skip inside
          (if (zerop (skip-syntax-forward "$" limit-close))  ;; closing $
              (goto-char limit)  ;; no closing $, or point before end of run
            (setq ret nil)))))   ;; closing $, we're outside
    ret))

(defun upstr-up-list-or-string--2 (arg)
  "An internal part of upstr.el.
ARG is 1 for forward or -1 for backward (the `up-list' style).
Go up out of either a sexp or maths delimiters at point,
whichever is innermost.

The approach is to do an (up-list -1) to go out of a sexp and
then examine between there and point to see whether we were in
fact within maths within that sexp."

  (let* ((sexp (save-excursion
                 (ignore-errors
                   (up-list -1)
                   (point)))) ;; pos start of sexp
         (math (upstr-math-start-position (or sexp
                                              (save-excursion
                                                (beginning-of-defun)
                                                (point))))))
    (if (not math)
        (up-list arg)

      ;; Go to ordinary sexp start if it's within the maths, otherwise go to
      ;; the maths position.
      (goto-char (max math (or sexp math)))

      (if (= arg 1)
          ;; forward to end of maths, or error if unterminated
          (forward-sexp)))))

(defun upstr-up-list-or-string--1 (arg)
  "An internal part of upstr.el.
ARG is 1 for forward or -1 for backward (the `up-list' style).
Go up out of a string or sexp/maths, whichever is innermost.
The sexp/maths movement is `upstr-up-list-or-string--2'.

If not within a string then go straight to that, otherwise
whether it or string starts later in the buffer."

  (let ((beg (upstr-string-start-position)))
    (if (not beg)
        (upstr-up-list-or-string--2 arg)

      ;; in a string, go up sexp/maths within the string
      (save-restriction ;; narrow to string extent
        (narrow-to-region beg
                          (or (save-excursion  ;; string end
                                (goto-char beg)
                                (ignore-errors (forward-sexp) (point)))
                              (point-max)))

        (unless (ignore-errors (upstr-up-list-or-string--2 arg) t)
          ;; could not go up within the string, go out of it
          (goto-char beg) ;; backward out of string
          (if (= arg 1)
              ;; forward to end of string, or error if unterminated
              (forward-sexp)))))))

;; Explicit autoload forms since cookies on the defuns don't work when
;; wrapped in the `upstr-interactive-nohat' macro.
;;
;;;###autoload (autoload 'upstr-backward-up-list-or-string "upstr" "Move point back and up a list or string level." t)
;;;###autoload (autoload 'upstr-up-list-or-string "upstr" "Move point up a list or string level." t)

(upstr-interactive-nohat
(defun upstr-backward-up-list-or-string (&optional arg)
  "Move point back and up a list or string level.
If point is within a pair of parens then move up to just before
the opening paren.

     (blah blah blah blah)
    ^         ^-- from here
    |--- to here

If point is not within parens but is in a string then move up to
just before its opening \" etc.

     \"blah blah blah\"
    ^        ^-- from here
    |--- to here

If point is not within parens but is in a TeX maths $..$ delimiter
pair then move up to just before its opening $.

     $$ x^2 + x + 1 = 0 $$
    ^        ^-- from here
    |--- to here

In each case the move is out of the \"innermost\" parens, string,
or maths.  (Except maths and strings don't occur together.  Maths
is only in `tex-mode' and there is no string syntax there.)

Optional ARG is how many levels to move up.

If ARG is 0 then don't move at all.  If ARG is negative then move
up and forward to the closing paren, quote, or $ as per
`upstr-up-list-or-string'.

This function uses `syntax-ppss' and occasionally its idea of
strings etc may be confused by changes to backslashing or
similar.  \\[normal-mode] is the easiest way to have it recheck."

  (interactive "^p")
  (upstr-up-list-or-string (- (or arg 1)))))

(upstr-interactive-nohat
(defun upstr-up-list-or-string (&optional arg)
  "Move point up a list or string level.
If point is within a pair of parens then move up to just after
the closing paren.

       (blah blah blah blah)
    from here--^            ^
                to here ----|

If point is not within parens but is in a string then move up to
just after its closing \" etc.

        \"blah blah blah\"
    from here---^       ^
             to here ---|

If point is not within parens but is in a TeX maths $..$ delimiter
pair then move up to just after its closing $.

     $$ x^2 + x + 1 = 0 $$
    from here---^         ^
               to here ---|

In each case the move is out of the \"innermost\" parens, string,
or maths.  (Except maths and strings don't occur together.  Maths
is only in `tex-mode' and there is no string syntax there.)

Strings are often start and end both \" or ' and TeX maths is
start and end $ or $$.  If you have an unmatched start then you
might be outside a string or maths when you thought you were
inside.  The result is usually a wild move up to some much bigger
enclosing parens.  In `latex-mode' this might be all the way up
to an enclosing \\=\\begin{document}.

Optional ARG is how many levels to move up.

If ARG is 0 then don't move at all.  If ARG is negative then move
up and back to the opening paren or quote as per
`upstr-backward-up-list-or-string'.

----
The upstr home page is
URL `http://user42.tuxfamily.org/upstr/index.html'"

  (interactive "^p")
  (unless arg (setq arg 1)) ;; default
  
  (let ((inc (if (> arg 0) 1 -1)))
    (while (/= 0 arg)
      (upstr-up-list-or-string--1 inc)
      (setq arg (- arg inc))))))

;;-----------------------------------------------------------------------------

;; LocalWords: upstr el docstring docstrings parens foo

(provide 'upstr)

;;; upstr.el ends here
