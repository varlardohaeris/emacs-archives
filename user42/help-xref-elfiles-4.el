;;; help-xref-elfiles.el --- buttonize foo.el and .emacs filenames in docstrings

;; Copyright 2011, 2012, 2013, 2014 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 4
;; Keywords: help
;; URL: http://user42.tuxfamily.org/help-xref-elfiles/index.html
;; EmacsWiki: CategoryHelp

;; help-xref-elfiles.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; help-xref-elfiles.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This spot of code buttonizes "foo.el" and ".emacs" filenames in
;; docstrings.
;;
;; foo.el files available in the `load-path' are buttonized so Ret
;; (`help-follow') visits the file, similar to the buttonized filename of
;; the source file for the function itself.
;;
;; .emacs is buttonized to visit `user-init-file'.  If you're running under
;; "emacs -q" then `user-init-file' is nil and the button throws an error.
;; Is there a way to find the usual init file when running -q ?
;;
;; Filenames in docstrings are uncommon in Emacs itself, but can be found in
;; add-on Lisp packages cross referencing each other.

;;; Emacsen:

;; Designed for Emacs 22 and up.  Requiries button.el and does nothing if
;; that's not available, which means nothing in Emacs 21 and XEmacs 21.

;;; Install:

;; Put help-xref-elfiles.el in one of your `load-path' directories and add
;; to your .emacs
;;
;;     (require 'help-xref-elfiles)
;;
;; To defer loading the code until using a help display then try instead
;;
;;     (eval-after-load "help-mode" '(require 'help-xref-elfiles))
;;
;; There's an autoload cookie below for this if you know how to use
;; `update-file-autoloads' and friends.

;;; History:

;; Version 1 - the first version
;; Version 2 - buttonize ".emacs" to go to `user-init-file'
;; Version 3 - oops, don't override `help-function-def' button
;; Version 4 - don't override `help-function-def' on .emacs too

;;; Code:

;;;###autoload (eval-after-load "help-mode" '(require 'help-xref-elfiles))

(when (eval-and-compile (or (featurep 'button)
                            (locate-library "button")))

(require 'button)

;; Must have advice.el for `ad-find-advice' macro when running uncompiled
;; (don't unload 'advice before `help-xref-elfiles-unload-function' runs)
(require 'advice)

(define-button-type 'help-xref-elfiles
  'action (lambda (button)
            (find-library (button-label button)))
  'follow-link t
  'help-echo "mouse-2, RET: go to lisp source file")

(define-button-type 'help-xref-elfiles-dotemacs
  'action (lambda (button)
            (unless user-init-file
              (error "No `user-init-file' set"))
            (find-file user-init-file))
  'follow-link t
  'help-echo "mouse-2, RET: go to .emacs file")

(defun help-xref-elfiles-button-in-region-p (beg end)
  "An internal part of help-xref-elfiles.el.
Return non-nil if there's any button between BEG and END.
END is exclusive, so a button starting at END is not considered."
  ;; `next-button' searches to the end of buffer, narrow to restrict it
  (save-restriction
    (narrow-to-region beg end)
    (next-button beg t))) ;; t to include any button at BEG

(defun help-xref-elfiles-buttonize (buffer)
  "An internal part of help-xref-elfiles.el.
Buttonize any available foo.el filenames in BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (let ((case-fold-search nil))
        (goto-char (point-min))

        ;; This pattern is words which might be filenames.  The separating
        ;; non-filename chars are roughtly whitespace and punctuation,
        ;; except want ".", "-" and ":" to be part of filenames, and maybe
        ;; other punctuation too, like unicode dashes.  For now the
        ;; non-filename chars are expressed explicitly, not by rather than
        ;; by char classes.  (Which also helps xemacs 21.4 which doesn't
        ;; have [[:punct:]] etc.
        ;;
        (while (re-search-forward "[^][(){}<>,;`'\" \t\r\n\f]+" nil t)
          (let ((beg      (match-beginning 0))
                (end      (match-end 0))
                (filename (match-string 0))
                type)

            ;; Trim trailing "." as full-stop rather than part of filename.
            (when (string-match "\\.+\\'" filename)
              (setq end      (+ beg (match-beginning 0)))
              (setq filename (substring filename 0 (match-beginning 0))))

            ;; "foo.el-style" trimmed to "foo.el", as found for example in
            ;; variable `muse-table-el-line-regexp' (in muse.el).
            (when (string-match "\\.el\\(-[a-z]+\\)\\'" filename)
              (setq end      (+ beg (match-beginning 1)))
              (setq filename (substring filename 0 (match-beginning 1))))

            (cond ((and (string-match "\\.el\\'" filename)
                        ;; Don't try talk to remote machines if some strange
                        ;; sample filename etc in a docstring.
                        (not (file-remote-p filename))
                        (locate-library filename))
                   (setq type 'help-xref-elfiles))

                  ((string-match "\\`\\(~/\\)?\\.emacs\\'" filename)
                   (setq type 'help-xref-elfiles-dotemacs)))

            (when (and type
                       ;; Don't override an existing button, in particular
                       ;; not the `help-function-def' for func/var source
                       ;; from M-x describe-function etc.
                       (not (help-xref-elfiles-button-in-region-p beg end)))
              (let ((inhibit-read-only t))
                (make-text-button beg end 'type type)))))))))

(defadvice help-make-xrefs (after help-xref-elfiles activate)
  "Buttonize foo.el filenames too."
  ;; fboundp check since emacs21/xemacs21 `unload-feature' unloads the
  ;; code but not the advice (though of course only if you've got a copy
  ;; of button.el to use this code there in the first place)
  (if (fboundp 'help-xref-elfiles-buttonize)
      (help-xref-elfiles-buttonize (ad-get-arg 0)))) ;; BUFFER arg

(defun help-xref-elfiles-unload-function ()
  "An internal part of help-xref-elfiles.el.
Remove advice from `help-make-xrefs'.
This is called by `unload-feature'."
  (when (ad-find-advice 'help-make-xrefs 'around 'help-xref-elfiles)
    (ad-remove-advice   'help-make-xrefs 'around 'help-xref-elfiles)
    (ad-activate        'help-make-xrefs))
  nil) ;; and do normal unload-feature actions too

)

;;  LocalWords:  el docstring docstrings buttonize buttonizes buttonized
;;  LocalWords:  filename filenames Ret foo init elfiles

(provide 'help-xref-elfiles)

;;; help-xref-elfiles.el ends here
