;;; diff-save-buffer.el --- default filename when saving a diff.

;; Copyright 2003, 2004, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2015 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 15
;; Keywords: files, diff
;; URL: http://user42.tuxfamily.org/diff-save-buffer/index.html
;; EmacsWiki: DiffSaveBuffer
;;
;; diff-save-buffer.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; diff-save-buffer.el is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; diff-save-buffer gives an initial .diff filename for a save-buffer in an
;; M-x diff or M-x vc-diff buffer, derived from the parent file being
;; diffed.  See the `diff-save-buffer' docstring below for more.

;;; Install:

;; Put diff-save-buffer.el in one of your `load-path' directories, and get
;; the M-x diff-save-buffer command with the following in your .emacs,
;;
;;     (autoload 'diff-save-buffer "diff-save-buffer" nil t)
;;
;; The intention is to bind it to C-x C-s in diff buffers, with for instance
;;
;;     (autoload 'diff-save-buffer-keybinding "diff-save-buffer")
;;     (add-hook 'diff-mode-hook 'diff-save-buffer-keybinding)
;;
;; Emacs 21 M-x diff uses compilation-mode instead of diff-mode, so a setup
;; there can be made too
;;
;;     (add-hook 'compilation-mode-hook 'diff-save-buffer-keybinding)
;;
;; There's autoload cookies for the functions and custom options if you
;; install via `M-x package-install' or know `update-file-autoloads'.  Then
;; add-hook or your own keybinding as desired.

;;; Emacsen:

;; Designed for Emacs 21 and up, works in XEmacs 21 and Emacs 20.
;;
;; XEmacs 21.4.20 had something fishy in M-x vc-diff where it used
;; fundamental-mode unless diff-mode had been loaded by something else
;; previously.

;;; History:
;;
;; Version 1 - the first version
;; Version 2 - recognise emacs22 M-x diff
;; Version 3 - use defadvice to cooperate with other read-file-name munging
;; Version 4 - autoload cookie meant to be on diff-save-buffer
;; Version 5 - for diff-buffer-with-file use origin file not the tempfile
;; Version 6 - undo defadvice on unload-feature
;; Version 7 - defang defadvice for emacs21,xemacs21 unload-feature
;; Version 8 - express dependency on 'advice
;; Version 9 - fix for xemacs21 read-file-name arg is `initial-contents'
;; Version 10 - emacs24 M-x diff filenames
;; Version 11 - emacs24 "mode: compilation" line is not diff filenames
;; Version 12 - set `default-filename' of read-file-name too
;; Version 13 - quieten xemacs byte compile
;; Version 14 - new email
;; Version 15 - cl for dolist in emacs20


;;; Code:

;; for `ad-find-advice' macro when running uncompiled
;; (don't unload 'advice before our -unload-function)
(require 'advice)

(defvar diff-old-file)
(defvar diff-new-file)
(defvar vc-parent-buffer)
(defvar temporary-file-directory)

(eval-when-compile
  (unless (fboundp 'dolist)
    (require 'cl))) ;; for `dolist' macro in emacs20

;;-----------------------------------------------------------------------------

(defun diff-save-buffer-unquote (obj)
  "Extract value from a quote form.
If OBJ is a list (quote X) then return X, otherwise return OBJ."
  (if (and (listp obj)
           (eq 'quote (car obj)))
      (cadr obj)
    obj))
(eval-when-compile
  (put 'diff-save-buffer-unquote 'side-effect-free t)
  (put 'diff-save-buffer-unquote 'pure t))

(defun diff-save-buffer-filenames ()
  "Return a pair (OLDNAME . NEWNAME) which are the files being diffed.
One or both names may be nil if they can't be determined,
including when the current buffer is not a diff.  For a `vc-diff'
OLDNAME is currently always nil even when the back-end does use
an actual file there."

  (or
   ;; `vc-diff' leaves the originating file buffer in a buffer local
   ;; vc-parent-buffer
   (and (boundp 'vc-parent-buffer)
        vc-parent-buffer
        (cons nil
              (buffer-file-name vc-parent-buffer)))

   ;; `diff' in emacs21 and emacs23 has buffer local variables diff-old-file
   ;; and diff-new-file for the filenames
   ;;
   ;; `diff' in xemacs21 has those variables too, but the values are pairs
   ;; (FILENAME . DELFLAG) instead of strings
   ;;
   (and (boundp 'diff-old-file)
        (cons (if (consp diff-old-file) (car diff-old-file) diff-old-file)
              (if (consp diff-new-file) (car diff-new-file) diff-new-file)))

   ;; `diff' in emacs22,23 doesn't seem to record the filenames except in a
   ;; generated lambda for revert-buffer-function, containing a call like
   ;;
   ;;     (diff (quote "oldname") (quote "newname") ...)
   ;;
   ;; dunno why the quoting, since the names should be strings; could eval
   ;; to get rid of it, but use cadr to avoid any risk of evaluating
   ;; something arbitrary
   ;;
   (and (listp revert-buffer-function)
        (let (found)
          (dolist (form revert-buffer-function)
            (and (listp form)
                 (eq 'diff (nth 0 form))
                 (setq found (cons (diff-save-buffer-unquote (nth 1 form))
                                   (diff-save-buffer-unquote (nth 2 form))))))
          found))


   ;; `diff' in emacs24 doesn't seem to record the filenames except in a
   ;; lexical-let of revert-buffer-function and process-sentinel, which are
   ;; too hard to pick apart.
   ;; Instead look for command line at start of buffer, eg
   ;;     diff -u oldfile newfile
   ;; The extract here doesn't cope with shell-quote-argument mangling applied.
   (save-excursion
     (goto-char (point-min))
     (and (not (looking-at "-\\*- mode:")) ;; not a "mode: compilation" line
          (looking-at ".* \\([^ \n]+\\) \\(.+\\)$")
          (cons (match-string 1) (match-string 2))))

   '(nil . nil)))

(defun diff-save-buffer-initial ()
  "Return initial filename for `diff-save-buffer' to offer."
  (let* ((pair (diff-save-buffer-filenames))
         (name (cdr pair)))

    ;; `diff-buffer-with-file' writes the buffer to /tmp/buffer-content-XXXX
    ;; and runs diff on that.  Use the "old" filename in that case.
    ;; Matching /tmp/buffer-content- like this isn't great, but as of emacs
    ;; 23.1 `diff-buffer-with-file' doesn't leave anything better.
    ;;
    ;; `diff-buffer-with-file' uses `make-temp-file' which uses only
    ;; `temporary-file-directory', no need to check also
    ;; `small-temporary-file-directory'.
    ;;
    (if (and name
	     (boundp 'temporary-file-directory) ;; not in xemacs21
             (equal temporary-file-directory (file-name-directory name))
             (string-match "\\`buffer-content-" (file-name-nondirectory name)))
        (setq name (car pair)))

    (and name
         (concat (file-name-nondirectory name) ".diff"))))

(defvar diff-save-buffer--initial nil
  "Temporary variable communicating with defadvice on `read-file-name'.")

;;;###autoload
(defun diff-save-buffer (&optional args)
  "`save-buffer' with an initial filename suggestion for a diff.
The proposed filename is the INITIAL argument to
`read-file-name', so it can be edited.  If `buffer-file-name' is
already set (eg. from already having saved), then that name is
used without further prompting, as usual for `save-buffer'.

In a `compilation-mode' buffer this function only proposes a
\".diff\" filename if it's a `vc-diff' (emacs21 uses
`compilation-mode' for `vc-diff').  For ordinary compiles nothing
special is done."
  ;; checkdoc-params: (args)

  (interactive)
  ;;
  ;; This is slightly nasty in that any other read-file-name happening under
  ;; save-buffer will also get the diff-save-buffer--initial applied.
  ;; Hopefully that doesn't happen in normal circumstances.  The aim is to
  ;; enhance the read, but leave everything else save-buffer does.
  ;;
  (let ((diff-save-buffer--initial (diff-save-buffer-initial)))
    (save-buffer args)))

;; Arg name is `initial' in emacs (20 up), but `initial-contents' in xemacs.
;; Use ad-get-arg/ad-set-arg 4 to allow for that difference.
;;
;; Arg 2 `default-filename' is used in emacs 24.3 when press Ret with no
;; change to the initial input, so set that to the target name too.  In
;; earlier emacs it was only necessary to set the `initial' arg.
;;
(defadvice read-file-name (before diff-save-buffer activate)
  "Initial value when called through `diff-save-buffer'."
  (when (and (boundp 'diff-save-buffer--initial) ;; in case `unload-feature'
             diff-save-buffer--initial
             (not buffer-file-name)
             (not (ad-get-arg 4))  ;; no DEFAULT-FILENAME already
             (not (ad-get-arg 4))) ;; no INITIAL already
    (ad-set-arg 2 diff-save-buffer--initial)   ;; DEFAULT-FILENAME
    (ad-set-arg 4 diff-save-buffer--initial))) ;; INITIAL

;; `-unload-function' only runs in emacs22 up, so the defadvice is made
;; harmless when everything else unloaded in emacs21 and xemacs21.
;; Removing the advice is good as a cleanup though.
;;
(defun diff-save-buffer-unload-function ()
  "Remove defadvice from `read-file-name'.
This is called by `unload-feature'."
  (when (ad-find-advice 'read-file-name 'before 'diff-save-buffer)
    (ad-remove-advice   'read-file-name 'before 'diff-save-buffer)
    (ad-activate        'read-file-name))
  nil) ;; and do normal unload-feature actions too

;;;###autoload
(defun diff-save-buffer-keybinding ()
  "Bind C-x C-s to `diff-save-buffer' in the current local keymap.
This function is designed for use from `diff-mode-hook', and
Emacs 21 `compilation-mode-hook'.

C-x C-s is normally `save-buffer'  in `global-map' to be `diff-save-buffer'
instead (see that function).

In Emacs 21 M-x vc-diff uses `diff-mode', but plain M-x diff uses
`compilation-mode', so to have C-x C-s there you should add to
`compilation-mode-hook' as well as `diff-mode-hook'."

  ;; might like to mangle the File/Save menu item [menu-bar file
  ;; save-buffer] so it goes to diff-save-buffer (and is enabled all the
  ;; time) in a diff buffer, but a merge/override of a global menu item from
  ;; a local keymap doesn't work

  (define-key (current-local-map) [?\C-x ?\C-s] 'diff-save-buffer))

;;;###autoload
(custom-add-option 'diff-mode-hook        'diff-save-buffer-keybinding)
;;;###autoload
(custom-add-option 'compilation-mode-hook 'diff-save-buffer-keybinding)

;; LocalWords: docstring filename diffed

(provide 'diff-save-buffer)

;;; diff-save-buffer.el ends here
