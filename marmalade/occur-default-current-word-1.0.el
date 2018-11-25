;;; occur-default-current-word.el --- Have M-x occur default to the word at point

;; Copyright (C) 2013 Nathaniel Flath <nflath@gmail.com>

;; Author: Nathaniel Flath <nflath@gmail.com>
;; URL: http://github.com/nflath/pager-default-keybindings
;; Version: 1.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; This package replaces the default suggestion for M-x occur (the last input) with the
;;; current word.

;;; Installation:

;; To install, put this file somewhere in your load-path and add the following
;; to your .emacs file:
;;(require ')

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

(defun occur-read-primary-args ()
  (list (read-regexp "List lines matching regexp"
                     (current-word))
        (when current-prefix-arg
          (prefix-numeric-value current-prefix-arg))))

(provide 'occur-default-current-word)
;;; occur-default-current-word.el ends here
