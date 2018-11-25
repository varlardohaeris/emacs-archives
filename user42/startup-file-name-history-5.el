;;; startup-file-name-history.el --- command line files into file-name-history

;; Copyright 2007, 2008, 2009, 2011, 2012, 2015 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 5
;; Keywords: files, history
;; URL: http://user42.tuxfamily.org/startup-file-name-history/index.html

;; startup-file-name-history.el is free software; you can redistribute it
;; and/or modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; startup-file-name-history.el is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This spot of code puts filenames on the Emacs command line into
;; `file-name-history', in addition to opening them in buffers in the usual
;; way.
;;
;; savehist.el can be used together with startup-file-name-history.el.
;; `savehist-mode' initializes `file-name-history' when .emacs loads, then
;; startup-file-name-history.el runs later during command line processing
;; and adds the command line files to whatever savehist loaded.

;;; Install:

;; Put startup-file-name-history.el in one of your `load-path' directories,
;; and in .emacs add
;;
;;     (require 'startup-file-name-history)
;;
;; Incidentally, `command-line-functions' is only a defvar, not a defcustom,
;; otherwise a custom-add-option on that variable could have been an easier
;; setup for novice users.

;;; Emacsen:

;; Designed for Emacs 20 and up.
;; Does nothing in XEmacs 21.4 (doesn't have `command-line-functions')

;;; History:

;; Version 1 - the first version
;; Version 2 - note ok with savehist.el
;; Version 3 - no autoload cookie, as it's a user preference really
;; Version 4 - absolutize by concat to preserve text, work in emacs20
;; Version 5 - new email


;;; Code:

(defun startup-file-name-history-add ()
  "Add command line filenames to `file-name-history'.
This function is designed for use from `command-line-functions'.
Global variable `argi' is expected to be a filename and is added
to `file-name-history'.  The return is nil, meaning argi has not
been consumed and should be passed to subsequent functions or the
default filename opener.

This function should be near the end of `command-line-functions',
after any handlers for strange non-filename arguments, so it sees
only filenames.

Filenames are made absolute for the history so they work from
later buffers with a different `default-directory'.  Abolutizing
is by a concat with `command-line-default-directory' rather than
`expand-file-name', since the concat preserves the name as typed
on the command line.  `command-line-default-directory' has the
usual abbreviation of the home directory as \"~/\" (when there),
keeping filenames shorter.

------
The startup-file-name-history.el home page is
URL `http://user42.tuxfamily.org/startup-file-name-history/index.html'"

  (let ((filename (if (file-name-absolute-p argi)
                      argi
                    (concat command-line-default-directory argi))))
    (if (eval-when-compile (fboundp 'add-to-history))

        ;; `add-to-history' is new in emacs22
        (add-to-history 'file-name-history filename)

      ;; emacs21 and earlier
      ;; Don't bother enforcing `history-length' here since
      ;; file-name-history should be either empty or quite short.  The first
      ;; interactive find-file will chop it if the command line was long.
      (setq file-name-history
            (cons filename file-name-history))))

  nil) ;; argi not consumed

;; XEmacs 21.4.22 doesn't have `command-line-functions'.  It's in the
;; manual, but not in the code.  Perhaps it'd be possible to notice the
;; initial command line `find-file' made from the loop in `command-line-1',
;; but that'd be fragile.
;;
;; Append to `command-line-functions' so we're near the end and hence only
;; see what reaches the default find-file handler, not strange things
;; crunched by other command-line-functions.
;;
;; `memq' and `append' for Emacs 20 since it doesn't have an APPEND arg to
;; `add-to-list'.
;; 
(when (eval-when-compile (boundp 'command-line-functions))
  (unless (memq 'startup-file-name-history-add command-line-functions)
    (setq command-line-functions
          (append command-line-functions
                  '(startup-file-name-history-add)))))

;;-----------------------------------------------------------------------------
;; LocalWords: filename filenames savehist el absolutize abolutizing

(provide 'startup-file-name-history)

;;; startup-file-name-history.el ends here
