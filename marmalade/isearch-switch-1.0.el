;;; isearch-switch.el --- switch the manner you are isearching in.

;; Copyright (C) 2014 Nathaniel Flath <flat0103@gmail.com>

;; Author: Nathaniel Flath <flat0103@gmail.com>
;; URL: http://github.com/nflath/isearch-switch
;; Version: 1.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file allows you to search how you are searching in the middle of the
;; search.  In other words, you can switch to searching by regexp, which
;; direction, or even if you would rather see all results in an occur buffer
;; instead.

;;; Installation

;; To use this mode, put the following in your init.el:
;; (require 'isearch-switch)

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

(defun isearch-switch-to-regexp (direction)
  "In isearch-mode, switch to searching by regular expression in
the given direction."
  (setq isearch-regexp t)
  (setq isearch-word nil)
  (setq isearch-success t isearch-adjusted t)
  (isearch-repeat direction)
  (isearch-update))

(defun isearch-switch-to-regexp-backward ()
  "In isearch-mode, switch to searching backward by regular
expression."
  (interactive)
  (isearch-switch-to-regexp 'backward))

(defun isearch-switch-to-regexp-forward ()
  "In isearch-mode, switch to searching forward by regular
expression."
  (interactive)
  (isearch-switch-to-regexp 'forward))

(defun isearch-switch-to-occur ()
  "In isearch-mode, switch to an 'occur-mode' buffer with the current search string."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))

(defvar isearch-switch nil
  "Whether isearch-switch has enabled keybindings." )

(defun isearch-switch-on ()
  "Turn on isearch-switch"
  (setq isearch-switch t)
  (define-key isearch-mode-map (kbd "C-M-r") 'isearch-switch-to-regexp-backward)
  (define-key isearch-mode-map (kbd "C-M-s") 'isearch-switch-to-regexp-forward)
  (define-key isearch-mode-map (kbd "C-o") `isearch-switch-to-occur))

(defun isearch-switch-off ()
  "Turn off isearch-switch"
  (setq isearch-switch nil)
  (define-key isearch-mode-map (kbd "C-M-r") nil)
  (define-key isearch-mode-map (kbd "C-M-s") nil)
  (define-key isearch-mode-map (kbd "C-o") nil))

;;;###autoload
(defun isearch-switch (&optional on)
  "If t, turn on isearch-switch.  If on is a negative number, turn off isearch-switch.  Else toggle."
  (cond ((eq on t) (isearch-switch-on))
        ((and (numberp on) (< on 0)) (isearch-switch-off))
        (isearch-switch (isearch-switch-off))
        ((not isearch-switch) (isearch-switch-on))))

;;;###autoload
(isearch-switch t)
(provide 'isearch-switch)
;;; isearch-switch.el ends here
