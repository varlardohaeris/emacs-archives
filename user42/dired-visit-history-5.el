;;; dired-visit-history.el --- add dired visited files to find-file history

;; Copyright 2009, 2011, 2012, 2015, 2016 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 5
;; Keywords: files, dired, history
;; URL: http://user42.tuxfamily.org/dired-visit-history/index.html
;; EmacsWiki: DiredMode

;; dired-visit-history.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; dired-visit-history.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a simple few lines to arrange that files visited from `dired'
;; with
;;
;;     Ret   dired-find-file
;;     v     dired-view-file
;;
;; are added to `file-name-history' and so are available as history in
;; `find-file' and other filename reading.
;;
;; Whether you want dired visits included in the find history is a matter of
;; personal preference.  Including them helps keep the history as all files
;; recently visited, whether you typed a name to C-x C-f or followed a dired
;; (or M-x locate) name.

;;; Emacsen:

;; Designed for Emacs 20 and up, works in XEmacs 21.

;;; Install:

;; Put dired-visit-history.el in one of your `load-path' directories and to
;; make `dired-visit-history-enable' available add to your .emacs,
;;
;;     (autoload 'dired-visit-history-enable "dired-visit-history")
;;
;; And to use it,
;;
;;     (add-hook 'dired-load-hook 'dired-visit-history-enable)
;;
;; `dired-load-hook' only runs when dired.el first loads, so if something in
;; your .emacs has already dragged in dired.el then make sure this
;; `add-hook' is early enough.  Or consider an `eval-after-load' or plain
;; `dired-mode-hook' instead.
;;
;; There's autoload cookies below if you know how to use
;; `update-file-autoloads' and friends.

;;; History:

;; Version 1 - the first version
;; Version 2 - use history-length in emacs21, as advised by Kevin Rogers
;; Version 3 - dired-view-file too
;; Version 4 - `remove' in emacs20
;; Version 5 - look for `dired-get-file-for-visit' at compile time

;;; Code:

;; Explicit dependency on advice.el because
;; `dired-visit-history-unload-function' needs `ad-find-advice' macro when
;; running not byte-compiled, and that macro is not autoloaded.
(require 'advice)

(eval-when-compile (require 'dired))

(eval-when-compile
  (unless (fboundp 'dolist)
    (require 'cl))) ;; for `dolist' in emacs20

(unless (fboundp 'remove)
  (require 'cl)) ;; for `remove' in emacs20

;; quieten byte compiler in opposite emacs
(defvar history-length)
(defvar minibuffer-history-minimum-string-length)
(defvar minibuffer-history-uniquify)


;;-----------------------------------------------------------------------------
;; generic

(if (eval-when-compile (fboundp 'add-to-history))
    ;; `add-to-history' new in emacs22
    (eval-and-compile ;; quieten the byte compiler
      (defalias 'dired-visit-history--add-to-history 'add-to-history))

  ;; emacs21, xemacs21
  ;; cf src/minibuf.c read_minibuf() in emacs
  ;; cf minibuf.el read-from-minibuffer in xemacs
  ;; crib: no `history-delete-duplicates' in emacs21/xemacs21
  ;;
  (defun dired-visit-history--add-to-history (histvar newval)
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

(defun dired-visit-history-add ()
  "An internal part of dired-visit-history.el.
Add the current `dired' filename to `file-name-history'.
This is for use in a `dired-mode' buffer."
  (let ((filename (if (eval-when-compile (fboundp 'dired-get-file-for-visit))
                      (dired-get-file-for-visit) ;; emacs22
                    (dired-get-filename))))      ;; emacs21
    (dired-visit-history--add-to-history 'file-name-history filename)))

(defadvice dired-find-file (before dired-visit-history disable)
  "Add visited files to `file-name-history'."
  ;; check fboundp since emacs21 where unload-feature removes the function
  ;; but doesn't run `dired-visit-history-unload-function' below to remove
  ;; the advice
  (if (fboundp 'dired-visit-history-add)
      (dired-visit-history-add)))

(defadvice dired-view-file (before dired-visit-history disable)
  "Add viewed files to `file-name-history'."
  ;; check fboundp since emacs21 where unload-feature removes the function
  ;; but doesn't run `dired-visit-history-unload-function' below to remove
  ;; the advice
  (if (fboundp 'dired-visit-history-add)
      (dired-visit-history-add)))

(defconst dired-visit-history-advice-list
  '(dired-find-file dired-view-file)
  "An internal part of dired-visit-history.el.
Functions which have `defadvice' from dired-visit-history.el.")

;;;###autoload
(defun dired-visit-history-enable ()
  "Arrange to add `dired' visited files to `file-name-history'.
This acts on `locate-mode' buffers too, since they're a variation
on `dired'."
  (interactive)
  (dolist (symbol dired-visit-history-advice-list)
    (ad-enable-advice symbol 'before 'dired-visit-history)
    (ad-activate      symbol)))

(defun dired-visit-history-disable ()
  "Don't add `dired' visited files to `file-name-history'."
  (interactive)
  (dolist (symbol dired-visit-history-advice-list)
    (ad-disable-advice symbol 'before 'dired-visit-history)
    (ad-activate       symbol)))

(defun dired-visit-history-unload-function ()
  "An internal part of dired-visit-history.el.
Remove advice applied to `dired-find-file'.
This is called by `unload-feature'."
  (dolist (symbol dired-visit-history-advice-list)
    (when (ad-find-advice symbol 'before 'dired-visit-history)
      (ad-remove-advice   symbol 'before 'dired-visit-history)
      (ad-activate        symbol)))
  nil) ;; and do normal unload-feature actions too

;;;###autoload
(custom-add-option 'dired-load-hook 'dired-visit-history-enable)

;; LocalWords: Ret filename el

(provide 'dired-visit-history)

;;; dired-visit-history.el ends here
