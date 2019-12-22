;;; emacs-visual-notifications.el --- A utility to create visual notifications in emacs -*- lexical-binding: t; -*-

;; This file is not part of Emacs

;; Author: Mohammed Ismail Ansari <team.terminal@gmail.com>
;; Version: 1.1
;; Package-Version: 20191120.1744
;; Keywords: library
;; Maintainer: Mohammed Ismail Ansari <team.terminal@gmail.com>
;; Created: 2017/09/07
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))
;; Description: A utility to create visual notifications in emacs
;; URL: http://ismail.teamfluxion.com
;; Compatibility: Emacs24


;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
;; for more details.
;;

;;; Install:

;; Put this file on your Emacs-Lisp load path and add the following to your
;; ~/.emacs startup file
;;
;;     (require 'emacs-visual-notifications)
;;
;; Currently, the following notifications are supported
;;
;; 1. Flash once
;;
;;     (emacs-visual-notifications-notify-short)
;;
;; The above function flashes the screen only once
;;
;; 2. Flash thrice
;;
;;     (emacs-visual-notifications-notify-long)
;;
;; The above function flashes the screen thrice
;;
;; 3. Flash a certain number of times
;;
;;     (emacs-visual-notifications-notify-times 5)
;;
;; The above function flashes the screen 5 times as specified
;;
;; 4. Flash until dismissed
;;
;;     (emacs-visual-notifications-notify-continuous)
;;
;; The above function flashes the screen until the user dismisses the
;; notification.
;;

;;; Commentary:

;;     You can use emacs-visual-notifications to create visual notifications
;;     of various kinds in emacs
;;
;;  Overview of features:
;;
;;     o   Create visual notifications to the user
;;

;;; Code:

(require 'cl-lib)

(defvar emacs-visual-notifications-timer
  nil)

(defvar emacs-visual-notifications-focusp
  t)

(defun emacs-visual-notifications-flash-half ()
  "Performs a half-flash"
  (invert-face 'fringe)
  (invert-face 'mode-line)
  (invert-face 'mode-line-inactive))

(defun emacs-visual-notifications-flash-once ()
  "Flashes screen once"
  (emacs-visual-notifications-flash-half)
  (run-at-time 0.7
               nil
               'emacs-visual-notifications-flash-half))

;;;###autoload
(defun emacs-visual-notifications-dismiss-notifications ()
  "Dismiss notifications"
  (interactive)
  (cond ((null emacs-visual-notifications-timer) (message "No notification to dismiss!"))
        (t (cancel-timer emacs-visual-notifications-timer))))

;;;###autoload
(defun emacs-visual-notifications-notify-short ()
  "Triggers a short notification"
  (if emacs-visual-notifications-focusp
      (emacs-visual-notifications-flash-once)
    (ding)))

;;;###autoload
(defun emacs-visual-notifications-notify-long ()
  "Triggers a long notification"
  (if emacs-visual-notifications-focusp
      (progn
        (emacs-visual-notifications-flash-once)
        (run-at-time 1.5
                     nil
                     (lambda ()
                       (emacs-visual-notifications-flash-once)
                       (run-at-time 1.5
                                    nil
                                    'emacs-visual-notifications-flash-once))))
    (ding)))

;;;###autoload
(defun emacs-visual-notifications-notify-times (count)
  "Triggers a notification for the specified number of times"
  (interactive)
  (let ((counter 0)
        (timer nil))
    (setq timer
          (run-at-time 0
                       1.5
                       (lambda ()
                         (emacs-visual-notifications-flash-once)
                         (cl-incf counter)
                         (cond ((= counter
                                   count) (cancel-timer timer))))))))

;;;###autoload
(defun emacs-visual-notifications-notify-continuous ()
  "Triggers a continuous notification until dismissed"
  (emacs-visual-notifications-dismiss-notifications)
  (setq emacs-visual-notifications-timer
        (run-at-time 0
                     1.5
                     'emacs-visual-notifications-flash-once)))

(add-hook 'focus-in-hook
          (lambda ()
            (setq emacs-visual-notifications-focusp
                  t)))
(add-hook 'focus-out-hook
          (lambda ()
            (setq emacs-visual-notifications-focusp
                  nil)))

(provide 'emacs-visual-notifications)

;;; emacs-visual-notifications.el ends here
