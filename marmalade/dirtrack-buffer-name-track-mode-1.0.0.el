;;; dirtrack-buffer-name-track-mode.el --- minor mode to cause shell buffers to reflect the working directory

;; Copyright (C) 2014 Nathaniel Flath <flat0103@gmail.com>

;; Author: Nathaniel Flath <flat0103@gmail.com>
;; Version: 1.0.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; dirtrack-buffer-name-track-mode causes your shell-mode buffers to reflect
;;; the working directory in their buffer name.

;;; Installation

;; To use this mode, put the following in your init.el:
;; (require 'dirtrack-buffer-name-track-mode)

;; This will be default turn fixme-mode on in all future prog-mode buffers.
;; To enable it in a specific buffer, run fixme-mode

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

(defvar dirtrack-buffer-name-track-prefix "shell-")
(defvar dirtrack-buffer-name-track-postfix "-shell")
(defvar dirtrack-buffer-name-track-max-prompt-len 40 "Maximum length of your prompt string.")

;; It is quite convenient to have your shell buffers contain the name of the
;; directory they are visiting.  This requires changing the buffer name whenever
;; dirtrack changes the working directory.
(defun dirtrack-buffer-name-track-directory-change-hook-fn ()
  "Change the name of the buffer to reflect the directory."
  (let* ((base-buffer-name (concat dirtrack-buffer-name-track-prefix
                                   (expand-file-name default-directory)
                                   dirtrack-buffer-name-track-postfix))
         (i 1)
         (full-buffer-name (concat base-buffer-name "<1>")))
    (if (get-buffer base-buffer-name)
        (progn
          (while (get-buffer full-buffer-name)
            (setq i (1+ i))
            (setq full-buffer-name (concat base-buffer-name "<" (number-to-string i) ">")))
          (rename-buffer full-buffer-name))
      (rename-buffer base-buffer-name))))

(define-minor-mode dirtrack-buffer-name-track-mode
  "Minor mode to cause shell-mode buffers change their name when the current working directory changes."
  :init-value t
  :group 'dirtrack-buffer-name-track-mode
  (if dirtrack-buffer-name-track-mode
      (add-hook 'dirtrack-directory-change-hook 'dirtrack-buffer-name-track-directory-change-hook-fn)
    (remove-hook 'dirtrack-directory-change-hook 'dirtrack-buffer-name-track-directory-change-hook-fn)))

(provide 'dirtrack-buffer-name-track-mode)
;;; dirtrack-buffer-name-track-mode.el ends here
