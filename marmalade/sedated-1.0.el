;;; sedated.el --- A few extremely simple sed functions for familiar text manipulation

;; Copyright (C) 2014 Andrew Hynes

;; Filename: sedated.el
;; Author: Andrew Hynes <andrewhynes@openmailbox.org>
;; URL: https://github.com/AndrewHynes/sedated.el
;; Description: A few extremely simple sed functions for familiar and potent text manipulation.
;; Version: 1.0
;; Keywords: sed, replace

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; As a foreword, sedated does not intend to work on Windows/non-Unix like
;; systems. You need a working shell and sed installed. Other than that,
;; all you need is Emacs. As conforming to the Unix-philosophy, this "library"
;; (again, used very loosely) does *NOT* reimplement sed. As such, running this
;; without sed will only result in errors.
;; Tested on GNU Emacs 24.3.1 on Arch GNU/Linux.
;;
;; The code's nothing impressive, but I hacked it together for myself and decided to
;; release it. I may wind up adding things in the future, but there's nothing else I 
;; can think of at the moment. It does everything it aims to. It's mainly intended
;; for people unfamiliar with Emacs Lisp.
;;
;; For instruction on using sed itself, see its man page. Type "man sed".
;; 
;; About sedated.el -
;;
;; A "library" (I use the term very lightly) of sed functions for Emacs
;; 
;; Adds the following functionality: 
;;
;;   - sedated-buffer and sedated-current-region for sed expressions
;;     in the buffer and region, respectively.
;; 
;;   - An easily extendable sedated-region function
;; 
;;   - Default bindings to M-s (buffer) and M-S (selected region)
;;     naturally, these can easily be changed by modifying
;;     (global-set-key (kbd "M-s") 'sedated-buffer) and
;;     (global-set-key (kbd "M-S") 'sedated-current-region)
;;     and sticking them in your .emacs.
;; 
;;   - Simple and does what it says on the tin.
;; 

(defun sedated-region (expression &optional start end)
  "Executes a sed expression on some text."
  (shell-command-on-region
   (if start start (point-min))
   (if end end (point-max))
   (concat "sed -e " expression)
   (current-buffer)
   t
   nil))

;;;###autoload
(defun sedated-buffer (expression)
  "Executes a sed expression on the full buffer after a prompt."
  (interactive (list (read-string "Enter sed expression: ")))
  (sedated-region expression))

;;;###autoload
(defun sedated-current-region (expression)
  "Executes a sed expression on the current region (selected text)."
  (interactive (list (read-string "Enter sed expression: ")))
  (sedated-region expression (point) (mark)))

;;;###autoload
(global-set-key (kbd "M-s") 'sedated-buffer)

;;;###autoload
(global-set-key (kbd "M-S") 'sedated-current-region)

;;; sedated.el ends here
