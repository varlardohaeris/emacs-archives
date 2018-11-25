;;; compile-history-local.el --- buffer local `compile-history'

;; Copyright 2010, 2011, 2012, 2013, 2014, 2015, 2016 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 1
;; Keywords: processes, compilation
;; URL: http://user42.tuxfamily.org/compile-history-local/index.html
;; EmacsWiki: CompilationMode

;; compile-history-local.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; compile-history-local.el is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This spot of code makes buffer-local compile commands and history.
;; Variables `compile-command' and `compile-history' become buffer local on
;; each M-x compile.  See `compile-history-local-enable' for more.

;;; Emacsen:

;; Designed for Emacs 21 and up and XEmacs 21 up.

;;; Install:

;; Put compile-history-local.el in one of your `load-path' directories, and
;; in your .emacs add
;;
;;     (require 'compile-history-local)
;;     (compile-history-local-enable)

;;; History:

;; Version 1 - the first version


;;; Code:

;; Explicit dependency on advice.el since
;; `compile-history-local-unload-function' needs `ad-find-advice'
;; macro when running not byte compiled, and that macro is not autoloaded.
(require 'advice)

(eval-when-compile
  (unless (fboundp 'dolist)
    (require 'cl))) ;; for `dolist' macro in emacs20

;;-----------------------------------------------------------------------------
;; compatibility

(if (eval-when-compile (fboundp 'add-to-history))
    ;; `add-to-history' new in emacs22
    (eval-and-compile ;; quieten the byte compiler
      (defalias 'compile-history-local--add-to-history 'add-to-history))

  ;; emacs21, xemacs21
  ;; cf src/minibuf.c read_minibuf() in emacs
  ;; cf minibuf.el read-from-minibuffer in xemacs
  ;; crib: no `history-delete-duplicates' in emacs21/xemacs21
  ;;
  (defun compile-history-local--add-to-history (histvar newval)
    ;; checkdoc-params: (histvar newval)
    "Add NEWVAL to history variable HISTVAR.
This is an equivalent to `add-to-history' of emacs22 up (but
without MAXELT or KEEP-ALL parameters).

In XEmacs `minibuffer-history-minimum-string-length' and
`minibuffer-history-uniquify' are obeyed."

    (let ((lst (symbol-value histvar)))
      (unless (or
               ;; non-lists quietly ignored
               (not (listp lst))

               ;; repeats never added
               (equal newval (car lst))

               ;; xemacs21 no small strings
               (and (boundp 'minibuffer-history-minimum-string-length)
                    minibuffer-history-minimum-string-length ;; if non-nil
                    (< (length newval)
                       minibuffer-history-minimum-string-length)))

        ;; xemacs21 uniquify.
        ;; cf `history-delete-duplicates' in emacs22 up.
        ;; As per xemacs21 this is not applied if `newelt' is not added.
        (if (or (and (boundp 'minibuffer-history-uniquify)
                     minibuffer-history-uniquify))
            ;; xemacs20,21 uses non-destructive `remove', do the same here
            (setq lst (remove newval lst)))

        (setq lst (cons newval lst))

        ;; Truncate to `history-length'.
        ;; Use a setcdr the same as emacs21 minibuf.c does.
        ;; No `history-length' in xemacs21.  Do its history lists grow
        ;; infinitely?
        (let ((maxlen (or (get histvar 'history-length)
                          (and (boundp 'history-length)
                               history-length))))
          (when (integerp maxlen)
            (if (zerop maxlen)
                (setq lst nil)
              (let ((tail (nthcdr (1- maxlen) lst)))
                (if (consp tail)
                    (setcdr tail nil))))))

        (set histvar lst)))))

;;-----------------------------------------------------------------------------

(defadvice compilation-start (after compile-history-local)
  "Compile history buffer-local."

  ;; (message "compile history %S add %S"
  ;;          (buffer-name ad-return-value)
  ;;          (substring-no-properties (ad-get-arg 0)))

  (with-current-buffer ad-return-value
    (set (make-local-variable 'compile-command) (ad-get-arg 0))
    (make-local-variable 'compile-history)
    (compile-history-local--add-to-history 'compile-history (ad-get-arg 0))))

;; (add-hook 'next-error-hook
;;           (lambda ()
;;             (message "next-error-hook in %S" (current-buffer))))

(defadvice next-error (after compile-history-local)
  "Compile history buffer-local."

  ;; (message "next-error in %S window %S" (current-buffer) (window-buffer))

  (let ((compile-buffer (if (boundp 'next-error-last-buffer)
                            next-error-last-buffer
                          compilation-last-buffer)))  ;; emacs20,xemacs21
    (when (eq (with-current-buffer compile-buffer major-mode)
              'compilation-mode) ;; not grep-mode or other derived
      (let ((command (with-current-buffer compile-buffer
                       (car compile-history))))

        ;; (message "next-error history %S to %S add %S"
        ;;          compile-buffer
        ;;          (window-buffer)
        ;;          (and command (substring-no-properties command)))

        (with-current-buffer (window-buffer)
          (set (make-local-variable 'compile-command) command)
          (make-local-variable 'compile-history)
          (compile-history-local--add-to-history 'compile-history command))))))

;; `-unload-function' only runs in emacs22 up, but that's ok since the
;; advice is harmless when everything else has unloaded because
;; `run-hook-with-args-until-success' just returns nil if the given hook
;; variable `compile-history-local-functions' has been unbound.
;;
(defun compile-history-local-unload-function ()
  "Remove advice on `compilation-start'.
This is called by `unload-feature'."
  (dolist (func '(compilation-start next-error))
    (when (ad-find-advice func 'after 'compile-history-local)
      (ad-remove-advice   func 'after 'compile-history-local)
      (ad-activate        func)))
  nil) ;; and do normal unload-feature actions too

;;;###autoload
(defun compile-history-local-enable ()
  "Enable buffer-local compile commands.
`compile-command' and its `compile-history' of previous commands
are made buffer-local.  This is good when working on different
things and so want to run and re-run different things from
different buffers.

When `next-error' goes to another buffer, the command from the
relevant compile is applied as a buffer-local `compile-command'
and `compile-history' in the new buffer too.  This is good for
re-running a command when following an error to another buffer
\(and fixing it).  This is done only when following errors from a
`compilation-mode' buffer, not derivatives like `grep-mode'.

Sometimes buffer local history becomes confusing if running
variations of a command.  The usual case is a single command run
repeatedly in a set of loosely related buffers.  Combinations of
`\\<minibuffer-local-map>\\[next-history-element]' or `\\<minibuffer-local-map>\\[previous-history-element]' can often find the wanted command.

The usual \"Local variables\" or major mode setups might already
set a buffer-local `compile-command'.  That remains the default
command.

----
The compile-history-local.el home page is
URL `http://user42.tuxfamily.org/compile-history-local/index.html'"

  (interactive)
  (dolist (func '(compilation-start next-error))
    (ad-enable-advice func 'after 'compile-history-local)
    (ad-activate      func)))

(defun compile-history-local-disable ()
  "Disable buffer-local `compile-history'.
Buffer-local-ness of `compile-history' in existing buffers is not
changed, but nothing new is applied."

  (interactive)
  (compile-history-local-unload-function))

;;-----------------------------------------------------------------------------

(provide 'compile-history-local)

;;; compile-history-local.el ends here
