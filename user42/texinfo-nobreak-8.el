;;; texinfo-nobreak.el --- texinfo line break workarounds for past makeinfo

;; Copyright 2006, 2007, 2008, 2009, 2010, 2012, 2015 Kevin Ryde
;;
;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 8
;; Keywords: tex, texinfo
;; EmacsWiki: Texinfo
;; URL: http://user42.tuxfamily.org/texinfo-nobreak/index.html

;; texinfo-nobreak.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; texinfo-nobreak.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; This spot of code avoids line breaks in texinfo source files at places
;; past versions of makeinfo didn't handle quite right.  Current makeinfo is
;; ok so this code is not needed unless you might use a past makeinfo.
;;
;; `texinfo-nobreak-enable' avoids line breaks by adding `texinfo-nobreak-p'
;; to `fill-nobreak-predicate', buffer-local.

;;; Emacsen:

;; Designed for Emacs 21 and up.  Works in Emacs 20.
;; Does nothing in XEmacs 21 as it doesn't have `fill-nobreak-predicate'.
;;
;; For Emacs 20 and 21 nobreak-fade.el is required, but not in Emacs 22 up.

;;; Install:

;; Put texinfo-nobreak.el in one of your `load-path' directories, and in
;; your .emacs add
;;
;;     (autoload 'texinfo-nobreak-enable "texinfo-nobreak" nil t)
;;     (add-hook 'texinfo-mode-hook 'texinfo-nobreak-enable)
;;
;; There's autoload cookies below for the function and a customize option on
;; `texinfo-mode-hook', if you know how to use `update-file-autoloads' and
;; friends.  The function can be used interactively to try it temporarily.

;;; History:

;; Version 1 - the first version
;; Version 2 - add some autoloads
;; Version 3 - skip multiple spaces for the benefit of auto-fill
;; Version 4 - new home page
;; Version 5 - new disable and use it for unload-feature
;; Version 6 - use nobreak-fade-add for emacs20,21
;; Version 7 - cl macros only when needed
;; Version 8 - makeinfo now ok

;;; Code:

(eval-when-compile
  (unless (fboundp 'dolist)
    (require 'cl))) ;; for `dolist' macro in emacs20

(defvar fill-nobreak-predicate) ;; quieten xemacs21 byte compiler


(defun texinfo-nobreak-p ()
  "Don't break after certain texinfo directives.
This function is designed for use in `fill-nobreak-predicate' and
can be set there with `texinfo-nobreak-enable'.

This function avoids a line break after the following two
directives which past makeinfo (circa its version 4.x) didn't
quite handle correctly.

- After \"@:\", since it's not obeyed at the end of a source
  line.

- After \"})\", since an external ref \"(@pxref{Fooing,,, foo,
  Foo Manual})\" at the end of a source line results in two
  spaces after \"...(foo)Fooing.)\".

Current makeinfo is ok and so these nobreaks are no longer
needed.  The easiest for the past versions was \"don't do that\"
in the source file, which this line break function ensures.

------
The \"})\" condition is broader than it needs to be.  Only an
@ref or @pxref going to an external manual is affected, but it's
a bit hard to distinguish those from other forms or other
commands.

Both conditions can be tricked by escapes @@ etc and give nobreak
at places which could break.  But @@: or @}) at the end of a line
will be unlikely and extra nobreaks in the source have no effect
on the final output."

  (save-excursion
    (skip-chars-backward " \t")
    (backward-char 2)
    (or (looking-at "@:\\|})"))))

;;;###autoload
(defun texinfo-nobreak-enable ()
  "Add `texinfo-nobreak-p' to `fill-nobreak-predicate'."
  (interactive)
  (cond ((not (or (eval-when-compile (boundp 'fill-nobreak-predicate))
                  (boundp 'fill-nobreak-predicate)))
         ;; xemacs21 -- no such feature at all, but recheck at runtime in
         ;; case an add-on creates something compatible
         )
        ((or (eval-when-compile (get 'fill-nobreak-predicate 'custom-type))
             (get 'fill-nobreak-predicate 'custom-type))
         ;; emacs22 up -- a hook, add to it buffer-local
         (add-hook 'fill-nobreak-predicate 'texinfo-nobreak-p
                   nil ;; no append
                   t)) ;; buffer-local
        (t
         ;; emacs20,21 -- fill-nobreak-predicate is a single-function variable
         (nobreak-fade-add 'texinfo-nobreak-p))))

;; In principle could add texinfo-nobreak-p as a customize option for
;; fill-nobreak-predicate in Emacs 22 where that variable is a hook.  But
;; it's highly texinfo specific and so unlikely to be wanted globally.
;;
;; (if (get 'fill-nobreak-predicate 'custom-type)
;;     (custom-add-option 'fill-nobreak-predicate 'texinfo-nobreak-p))
;;
;; Instead just show texinfo-nobreak-enable on texinfo-mode-hook with the
;; following:
;;
;;;###autoload
(custom-add-option 'texinfo-mode-hook 'texinfo-nobreak-enable)

(defun texinfo-nobreak-disable ()
  "Remove `texinfo-nobreak-p' from `fill-nobreak-predicate'."

  ;; this is a bit excessive, but for emacs22 up it's just the `remove-hook'
  ;; and the byte compile optimizer turfs the rest
  (interactive)
  (cond ((not (or (eval-when-compile (boundp 'fill-nobreak-predicate))
                  (boundp 'fill-nobreak-predicate)))
         ;; xemacs21 -- no such feature at all, but recheck at runtime in
         ;; case an add-on creates something compatible
         )

        ((or (eval-when-compile (get 'fill-nobreak-predicate 'custom-type))
             (get 'fill-nobreak-predicate 'custom-type))
         ;; emacs22 up -- a hook, remove buffer-local setting
         (remove-hook 'fill-nobreak-predicate 'texinfo-nobreak-p
                      t)) ;; buffer-local

        ;; emacs21 -- fill-nobreak-predicate is a variable holding a function
        ((eq fill-nobreak-predicate 'texinfo-nobreak-p)
         ;; work on global value too if accidentally got in there
         (setq fill-nobreak-predicate nil)
         (kill-local-variable 'fill-nobreak-predicate))
        ((not (local-variable-p 'fill-nobreak-predicate (current-buffer)))
         )
        ;; remove from (lambda () (or ...))
        ((and (listp fill-nobreak-predicate)
              (eq 'lambda (car-safe fill-nobreak-predicate))
              (= 3 (length fill-nobreak-predicate))
              (eq 'or (car-safe (nth 2 fill-nobreak-predicate))))
         (let ((lst (cdr (nth 2 fill-nobreak-predicate))))
           (when (member '(texinfo-nobreak-p) lst)
             (setq lst (delete '(texinfo-nobreak-p) (copy-sequence lst)))
             (if (null lst)
                 (progn
                   ;; work on global value too if accidentally got in there
                   (setq fill-nobreak-predicate nil)
                   (kill-local-variable 'fill-nobreak-predicate))
               (setq fill-nobreak-predicate
                     `(lambda () (or ,@lst)))))))))

(defun texinfo-nobreak-unload-function ()
  "Remove `texinfo-nobreak-p' from `fill-nobreak-predicate' on unload.
As of Emacs 23.1 `unload-feature' doesn't remove functions from
buffer-local values of hooks, and nor is `fill-nobreak-predicate'
in `unload-feature-special-hooks', so remove explicitly."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (texinfo-nobreak-disable)))
  nil) ;; and normal unload-feature actions

;; LocalWords: texinfo makeinfo filladapt pxref Fooing nobreak nobreaks el
;; LocalWords: foo Foo bu

(provide 'texinfo-nobreak)

;;; texinfo-nobreak.el ends here
