;;; buffer-file-utils.el --- utilities operating on a buffer and the associated file

;; Copyright (C) 2014 Nathaniel Flath <flat0103@gmail.com>

;; Author: Nathaniel Flath <flat0103@gmail.com>
;; Version: 1.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; buffer-file-utils provides utility functions for operating on both a buffer
;; and it's associated file.  The functions provided are
;; 'rename-file-and-buffer' and 'move-buffer-file'.  'rename-file-and-buffer'
;; will rename the buffer and file to the same thing, and move-buffer-file will
;; change the directory the buffer/file is in.

;;; Installation

;; To use this mode, put the following in your init.el:
;; (require 'buffer-file-utils)

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun move-buffer-file (dir)
  "Move both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn
        (copy-file filename newname 1)
        (delete-file filename)
        (set-visited-file-name newname)
        (set-buffer-modified-p nil)     t))))

(provide 'buffer-file-utils)
;;; buffer-file-utils.el ends here
