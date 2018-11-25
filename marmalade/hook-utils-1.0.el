;;; hook-utils.el --- Add a few utility functions for manipulating hooks

;; Copyright (C) 2013 Nathaniel Flath <nflath@gmail.com>

;; Author: Nathaniel Flath <nflath@gmail.com>
;; URL: http://github.com/nflath/hook-utils
;; Version: 1.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; This package defines a few functions useful for manipulating hooks

;;; Installation:

;; To install, put this file somewhere in your load-path and add the following
;; to your .emacs file:
;; (require 'hook-utils)

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

;;Use the common-lisp library.
(require 'cl)

(defun hook-utils-add-hook-to-all (hooks fn)
  "Add a function to a list of hooks."
  (mapcar (lambda (hook) (add-hook hook fn)) hooks))

(defun hook-utils-remove-hook-from-all (hooks fn)
  "Removes a function from a list of hooks."
  (mapcar (lambda (hook) (remove-hook hook fn)) hooks))

(provide 'hook-utils)
;;; hook-utils.el ends here
