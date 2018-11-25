;;; winpoint-ignore-dired.el --- Have winpoint ignore dired-mode buffers

;; Copyright (C) 2013 Nathaniel Flath <nflath@gmail.com>

;; Author: Nathaniel Flath <nflath@gmail.com>
;; URL: http://github.com/nflath/pager-default-keybindings
;; Version: 1.1
;; Package-Requires: ((winpoint "1.0"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; This package changes winpoint-restore to ignore dired-mode buffers, due to
;;; a bug where the point will be 0 if you call dired on a buffer you already
;;; have open.

;;; Installation:

;; To install, put this file somewhere in your load-path and add the following
;; to your .emacs file:
;; (require 'winpoint-ignore-dired)

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

(require 'winpoint)

(defun winpoint-restore (win)
  "Restore point in the window WIN."
  (with-selected-window win
    (let ((point (winpoint-get win (current-buffer))))
      (when (and point
               (not (eq major-mode 'dired-mode)))
        (goto-char point)))))

(winpoint-mode t)

(provide 'winpoint-ignore-dired)
;;; winpoint-ignore-dired.el ends here
