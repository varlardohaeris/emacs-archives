;;; makeinfo-info.el --- use Info-mode with makeinfo-buffer.

;; Copyright (C) 2002, 2004, 2005, 2007, 2009, 2010, 2015 Kevin Ryde
;;
;; Copyright (C) 1991, 1993, 1994, 1997, 1999, 2000, 2001 Free Software
;; Foundation, Inc.

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 9
;; Keywords: docs, tex, texinfo
;; EmacsWiki: MakeinfoInfo
;; URL: http://user42.tuxfamily.org/makeinfo-info/index.html

;; makeinfo-info.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; makeinfo-info.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This code extends `makeinfo-buffer' (C-c C-m C-b) in two ways,
;;
;;     1. Display its result with `Info-mode', rather than a raw buffer.
;;
;;     2. Follow a TeX-master variable for the top-level .texi of a
;;        multi-file document.
;;
;; In Emacs 22 item 1 has been adopted, so just TeX-master is added there.
;; See the docstring of `makeinfo-info-makeinfo-buffer' below for details.
;;
;; `makeinfo-region' (C-c C-m C-r) is unchanged, it still shows the raw
;; foo.info in fundamental mode.  This is probably good enough, since
;; usually only small sections of a document can be processed in isolation
;; anyway.

;;; Install:

;; Put makeinfo-info.el in one of your `load-path' directories, and in your
;; .emacs add
;;
;;     (eval-after-load "makeinfo" '(require 'makeinfo-info))
;;
;; There's an autoload cookie below for this, if you use
;; `update-file-autoloads' and friends.

;;; Emacsen:

;; Designed for Emacs 21 and 22, works in XEmacs 21 too.

;;; History:

;; Version 1 - the first version.
;; Version 2 - look for TeX-master.
;; Version 3 - correction to info file reverting.
;; Version 4 - notice emacs 22, only add TeX-master support there.
;; Version 5 - eval-when-compile to omit the emacs21 bits on later emacs
;; Version 6 - xemacs eval-after-load only takes a string
;; Version 7 - undo defadvice on unload-feature
;; Version 8 - express dependency on 'advice
;; Version 9 - new email


;;; Code:

;;;###autoload (eval-after-load "makeinfo" '(require 'makeinfo-info))
;; checkdoc-autoload: no-entrypoint

(require 'info)
(require 'makeinfo)

;; Explicit dependency on advice.el since `makeinfo-info-unload-function'
;; needs `ad-find-advice' macro when running not byte compiled, and that
;; macro is not autoloaded.
(require 'advice)


;; defined by AUCTeX, or something
(defvar TeX-master)

(unless (eval-when-compile (fboundp 'Info-revert-find-node))
  ;; the following is for emacs21 and xemacs21, it's already in emacs22

  (defvar makeinfo-output-node-name nil
    "Node name to visit in output file, for `makeinfo-buffer'.")

  ;; It's perhaps a bit nasty to kill the *info* buffer to force a re-read,
  ;; but at least it keeps this routine (which is only for the benefit of
  ;; makeinfo-buffer) out of the way of normal operations.
  ;;
  (defun Info-revert-find-node (filename nodename)
    "Go to an info node FILENAME and NODENAME, re-reading disk contents.
When *info* is already displaying FILENAME and NODENAME, the window position
is preserved, if possible."

    ;; Use the current window for *info*, if it's not already in a window,
    ;; since the "other" window will be the compile output.  In emacs21 this
    ;; already happens because info is in the default `same-window-regexps',
    ;; but not in xemacs21.  In any case force it so we get the two windows
    ;; info and compile.
    (let ((same-window-buffer-names (cons "*info*" same-window-buffer-names)))
      (pop-to-buffer "*info*"))

    (let ((old-filename Info-current-file)
          (old-nodename Info-current-node)
          (pcolumn      (current-column))
          (pline        (count-lines (point-min) (line-beginning-position)))
          (wline        (count-lines (point-min) (window-start)))
          (old-history  Info-history)
          (new-history (and Info-current-file
                            (list Info-current-file Info-current-node (point)))))
      (kill-buffer nil)
      (Info-find-node filename nodename)
      (setq Info-history old-history)
      (if (and (equal old-filename Info-current-file)
               (equal old-nodename Info-current-node))
          (progn
            ;; note goto-line is no good, we want to measure from point-min
            (goto-char (point-min))
            (forward-line wline)
            (set-window-start (selected-window) (point))
            (goto-char (point-min))
            (forward-line pline)
            (move-to-column pcolumn))
        ;; only add to the history when coming from a different file+node
        (if new-history
            (setq Info-history (cons new-history Info-history))))))

  (defun makeinfo-current-node ()
    "Return the name of the node containing point."
    (save-excursion
      (end-of-line)           ; in case point is at the start of an @node line
      (if (re-search-backward "^@node\\s-+\\([^,\n]+\\)" (point-min) t)
          (match-string 1)
        "Top")))

  ;; replace makeinfo.el code
  ;;
  (defun makeinfo-compilation-sentinel (proc msg)
    (compilation-sentinel proc msg)
    (when (memq (process-status proc) '(signal exit))
      (if (and makeinfo-temp-file (file-exists-p makeinfo-temp-file))
          ;; makeinfo-region, old style
          (progn
            (delete-file makeinfo-temp-file)
            ;; Always use the version on disk.
            (let ((buffer (get-file-buffer makeinfo-output-file-name)))
              (if buffer
                  (with-current-buffer buffer
                    (revert-buffer t t))
                (setq buffer (find-file-noselect makeinfo-output-file-name)))
              (if (window-dedicated-p (selected-window))
                  (switch-to-buffer-other-window buffer)
                (switch-to-buffer buffer)))
            (goto-char (point-min)))

        ;; makeinfo-buffer, new style
        ;;
        ;; makeinfo-output-file-name is usually just "foo.info" and must be
        ;; expanded before we switch to the *info* buffer, because
        ;; default-directory might be different there.
        ;;
        (let ((filename (expand-file-name makeinfo-output-file-name)))
          (if (file-exists-p filename)
              (Info-revert-find-node filename makeinfo-output-node-name)))))))

;; This code derived from makeinfo-buffer in makeinfo.el.
;;
;; In the docstring "\\<Info-mode-map>\\[Info-last]" doesn't show "l" in
;; emacs22 because Info-last has become and alias for `Info-history-back'.
;; So just write "l" explicitly.
;;
(defun makeinfo-info-makeinfo-buffer ()
  "Make Info file from current buffer by running \"makeinfo\".
`Info-mode' is used to display the output and the node shown is
the current one from the .texi buffer.  When re-running makeinfo
from that same node the Info buffer window position is
maintained, which is good for seeing small changes as you edit.

If you didn't want to go to the current node then the usual
`Info-last'(l) can return to where you were.

Errors from makeinfo are shown in a compile buffer.  Use
\\[next-error] in the usual way to go to the offending part of
the source.

For a multi-file document, set a `TeX-master' local variable to
indicate the toplevel file, ie. the one \"makeinfo\" should run
on.  For example in the local variables section of each subfile,

    @c Local\
 variables:
    @c TeX-master: \"mytopfile.texi\"
    @c End:

In Emacs 22 TeX-master is classed as \"risky\", because it could
make you run makeinfo on a strange file, and that file could have
a malicious @setfilename, so you'll be prompted to accept the
setting.  Alternately you can write a couple of lines of lisp and
set TeX-master from `find-file-hooks' or similar.  This is good
for covering all the files in a directory, or if the files are
say a cvs checkout and you don't want to modify them.  If you
only work on one document at a time then of course you can just
`setq' or \\[set-variable] too."

  (interactive)
  (cond ((boundp 'TeX-master)
         (save-some-buffers))  ;; buffers might be holding sub-files
        ((null buffer-file-name)
         (error "Buffer not visiting any file"))
        ((buffer-modified-p)
         (if (y-or-n-p "Buffer modified; do you want to save it? ")
             (save-buffer))))

  (let ((master (if (and (boundp 'TeX-master)     ;; usually as a buffer-local
                         (not (eq t TeX-master))) ;; auctex meaning current buf
                    TeX-master
                  buffer-file-name)))

    ;; Find and record the Info filename,
    ;; or else explain that a filename is needed.
    ;;
    ;; If the master file is not in a buffer then it's only visited
    ;; temporarily, so as not to junk up the user's buffer list.
    (save-excursion
      (let ((buf (get-file-buffer master)))
        (if buf
            (set-buffer buf)
          (find-file master))
        (goto-char (point-min))
        (unwind-protect
            (let ((search-end (save-excursion (forward-line 100) (point))))
              (if (re-search-forward
                   "^@setfilename[ \t]+\\([^ \t\n]+\\)[ \t]*"
                   search-end t)
                  (setq makeinfo-output-file-name
                        (expand-file-name
                         (buffer-substring (match-beginning 1) (match-end 1))))
                (error "%s needs a line saying: @setfilename <name>" master)))
          (unless buf (kill-buffer nil)))))
    (setq makeinfo-output-node-name (makeinfo-current-node))

    (save-excursion
      (if (fboundp 'makeinfo-next-error)
          ;; emacs22 takes args COMMAND, DISABLE-ERRORS (flag), SENTINEL
          (makeinfo-compile
           (concat makeinfo-run-command " " makeinfo-options " " master)
           nil
           'makeinfo-compilation-sentinel-buffer)

        ;; emacs21 takes args COMMAND, ERROR-MESSAGE, PARSE-ERRORS
        (makeinfo-compile
         (concat makeinfo-run-command " " makeinfo-options " " master)
         "No more errors."
         t)))))

(defadvice makeinfo-buffer (around makeinfo-info last activate)
  "Override to use `makeinfo-info-makeinfo-buffer' instead."
  ;; no defined return value, but put through whatever comes back
  (setq ad-return-value (makeinfo-info-makeinfo-buffer)))

(defun makeinfo-info-unload-function ()
  "Remove defadvice from `makeinfo-buffer'.
This is called by `unload-feature'."
  (when (ad-find-advice 'makeinfo-buffer 'around 'makeinfo-info)
    (ad-remove-advice   'makeinfo-buffer 'around 'makeinfo-info)
    (ad-activate        'makeinfo-buffer))
  nil) ;; and do normal unload-feature actions too

;; LocalWords: docstring toplevel mytopfile texi makeinfo ie subfile
;; LocalWords: setfilename cvs

(provide 'makeinfo-info)

;;; makeinfo-info.el ends here
