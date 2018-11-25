;;; grr.el --- Simple Growl notifications for Emacs and Mac OS X

;; Copyright (C) 2012  David Leatherman

;; Author: David Leatherman <leathekd@gmail.com>
;; URL: http://www.github.com/leathekd/grr.el
;; Version: 1.0.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; grr.el is a simple wrapper over growlnotify and will not work if
;; growlnotify is missing.

;; Currently only supports setting the title, message, and sticky flag.

;; History

;; 1.0.0 - Initial release.

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
(defvar grr-app "Emacs.app")

(defvar grr-command (executable-find "growlnotify")
  "The path to growlnotify")

(defun grr-clean-string (s)
  "Trims and collapses whitespace"
  (replace-regexp-in-string "^ +\\| +$" ""
                            (replace-regexp-in-string " +" " " s)))

(defun grr-encode-string (s)
  (encode-coding-string s (keyboard-coding-system)))

(defun grr-notify (title msg &optional sticky notification-name)
  "Shows a message through the growl notification system using
  `grr-command' as the program."
  (if grr-command
      (let ((process (start-process "grr" nil grr-command
                                    (grr-encode-string (grr-clean-string title))
                                    "-a" grr-app
                                    (if (null sticky) "" "--sticky"))))
        (process-send-string process (grr-encode-string (grr-clean-string msg)))
        (process-send-string process "\n")
        (process-send-eof process)
        t)
    (error "Could not find growlnotify.")))

(defun grr-toggle-notifications ()
  "Turn off or on visual growl notifications.  Growl will remain running."
  (interactive)
  (let* ((cmd "defaults read com.growl.growlhelperapp GrowlSquelchMode")
         (squelched (string= "1" (replace-regexp-in-string
                                  "\n" "" (shell-command-to-string cmd)))))
    (if squelched
        (progn
          (shell-command-to-string
           "defaults write com.Growl.GrowlHelperApp GrowlSquelchMode -bool NO")
          (message "Growl On"))
      (progn
        (shell-command-to-string
         "defaults write com.Growl.GrowlHelperApp GrowlSquelchMode -bool YES")
        (message "Growl Off")))
    (shell-command-to-string cmd)))

(provide 'grr)
;;; grr.el ends here
