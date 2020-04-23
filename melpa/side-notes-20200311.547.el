;;; side-notes.el --- Easy access to a directory notes file  -*- lexical-binding: t; -*-

;; Copyright (c) 2019-2020  Paul William Rankin

;; Author: William Rankin <code@william.bydasein.com>
;; Keywords: convenience
;; Package-Version: 20200311.547
;; Version: 0.3.1
;; Package-Requires: ((emacs "24.5"))
;; URL: https://github.com/rnkn/side-notes

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; # Side Notes #

;; Quickly display your quick side notes in quick side window.

;; Side notes live in a file in the current directory or any parent
;; directory thereof and is displayed in a side window with
;; side-notes-toggle-notes. The filename to look for is defined by user
;; option side-notes-file, which defaults to "notes.txt".

;; To really mix things up, there's the optional
;; side-notes-secondary-file, which, when non-nil, will display a
;; separate notes file in a lower side window when the command
;; side-notes-toggle-notes is prefixed with an argument (C-u).

;; For more info, see (info "(elisp) Side Windows")

;; ## Installation ##

;; Install from [MELPA stable] then add something like the following to
;; your init file:

;;     (define-key (current-global-map) (kbd "M-s n") #'side-notes-toggle-notes)

;; [melpa stable]: https://stable.melpa.org/#/side-notes

;;; Code:

(defgroup side-notes ()
  "Display a notes file."
  :group 'convenience)

(defcustom side-notes-hook
  nil
  "Hook run after showing notes buffer."
  :type 'hook
  :group 'side-notes)

(defcustom side-notes-file
  "notes.txt"
  "Name of the notes file.

This file lives in the current directory or any parent directory
thereof, which allows you to keep a notes file in the top level
of a multi-directory project.

If you would like to use a file-specific notes file, specify a
string with `add-file-local-variable'. Likewise you can specify a
directory-specific notes file with `add-dir-local-variable'."
  :type 'string
  :safe 'stringp
  :group 'side-notes)
(make-variable-buffer-local 'side-notes-file)

(defcustom side-notes-secondary-file
  nil
  "Name of an optional secondary notes file.

Like `side-notes-file' but displayed when `side-notes-toggle-notes'
is prefixed with \\[universal-argument].

If you would like to use a file-specific notes file, specify a
string with `add-file-local-variable'. Likewise you can specify a
directory-specific notes file with `add-dir-local-variable'."
  :type '(choice (const nil)
                 (string "notes-2.txt"))
  :safe 'stringp
  :group 'side-notes)
(make-variable-buffer-local 'side-notes-secondary-file)

(defcustom side-notes-select-window
  t
  "If non-nil, switch to notes window upon displaying it."
  :type 'boolean
  :safe 'booleanp
  :group 'side-notes)

(defcustom side-notes-display-alist
  '((side . right)
    (window-width . 35))
  "Alist used to display notes buffer.

See `display-buffer-in-side-window' for example options.

n.b. the special symbol `slot' added automatically to ensure that
`side-notes-file' is displayed above `side-notes-secondary-file'."
  :type 'alist
  :group 'side-notes)

(defface side-notes
  '((t nil))
  "Default face for notes buffer."
  :group 'side-notes)

(defvar-local side-notes-buffer-identify
  nil
  "Buffer local variable to identify a notes buffer.")

(defun side-notes-locate-notes (&optional arg)
  "Look up directory hierachy for file `side-notes-file'.

Return nil if no notes file found."
  (let ((file (if (and arg side-notes-secondary-file)
                  side-notes-secondary-file
                side-notes-file)))
    (expand-file-name file (locate-dominating-file default-directory file))))

;;;###autoload
(defun side-notes-toggle-notes (arg)
  "Pop up a side window containing `side-notes-file'.

When ARG is non-nil (if prefixed with \\[universal-argument]), locate
`side-notes-secondary-file' instead.

See `side-notes-display-alist' for options concerning displaying
the notes buffer."
  (interactive "P")
  (if side-notes-buffer-identify
      (quit-window)
    (let ((display-buffer-mark-dedicated t)
          (buffer (find-file-noselect (side-notes-locate-notes arg))))
      (if (get-buffer-window buffer (selected-frame))
          (delete-windows-on buffer (selected-frame))
        (display-buffer-in-side-window
         buffer (cons (cons 'slot (if arg 1 -1)) side-notes-display-alist))
        (with-current-buffer buffer
          (setq side-notes-buffer-identify t)
          (face-remap-add-relative 'default 'side-notes)
          (run-hooks 'side-notes-hook))
        (if side-notes-select-window
            (select-window (get-buffer-window buffer (selected-frame))))
        (message "Showing `%s'; %s to hide" buffer
                 (key-description (where-is-internal this-command
                                                     overriding-local-map t)))))))

(provide 'side-notes)
;;; side-notes.el ends here

;; Local Variables:
;; coding: utf-8-unix
;; fill-column: 80
;; require-final-newline: t
;; sentence-end-double-space: nil
;; End:
