;;; evil-ex-shell-command.el --- invoke shell-command right from evil-ex  -*- lexical-binding: t; -*-

;; Copyright © 2018, Rashawn Zhang, all rights reserved.

;; Version: 0.1.0
;; Package-Version: 20180903.614
;; URL: https://github.com/yqrashawn/evil-ex-shell-command
;; Package-Requires: ((emacs "24.4") (evil "1.1.0"))
;; Author: Rashawn Zhang <namy.19@gmail.com>
;; Created:  2 August 2018
;; Keywords: tools, shell-command, evil

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;  Invoke `shell-command' right from `evil-ex'

;;; Code:

(require 'evil)

(defvar evil-ex-shell-command-prefix ";"
  "Prefix for ‘evil-ex’ command that will invoke `shell-command'.")

(defun evil-ex-shell-command-eval (orig-fun str)
  "Advice for ‘evil-ex-execute’. ORIG-FUN is `evil-ex-execute', STR is the command input."
  (if (not (cond
            ((string-prefix-p evil-ex-shell-command-prefix str)
             (funcall #'shell-command
                      (string-remove-prefix evil-ex-shell-command-prefix str)))))
      (funcall orig-fun str)))

(advice-add #'evil-ex-execute :around 'evil-ex-shell-command-eval)

;;;###autoload
(defun evil-ex-shell-command ()
  "Invoke `evil-ex' with `evil-ex-shell-command-prefix'."
  (interactive)
  (evil-ex evil-ex-shell-command-prefix))

(provide 'evil-ex-shell-command)
;;; evil-ex-shell-command.el ends here
