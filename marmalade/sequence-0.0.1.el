;;; sequence.el --- makes sequences of numbers -*- lexical-binding: t -*-

;; Copyright (C) 2013  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: lisp
;; Version: 0.0.1
;; Url: http://github.com/nicferrier/emacs-sequence

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

;;; Commentary:

;; Very simple tool to make a sequence of numbers.  Probably only
;; makes sense inside an actor framework.

;;; Code:

(defun sequence-maker (file &optional starting-point)
  "Make a procedure to make numbers.

FILE is the storage for this number sequence.

STARTING-POINT is an arbitary starting point.  If the file is
present then that will be used instead to start the sequence."
  (let* ((counter (or starting-point 0))
         (buf (or (get-buffer (file-name-nondirectory file))
                  (with-current-buffer (find-file-noselect file)
                    (goto-char (point-min))
                    (setq
                     counter
                     (condition-case err
                         (read (current-buffer))
                       (end-of-file counter)))
                    (current-buffer)))))
    ;; Return the proc that will be the counter
    (lambda ()
      (setq counter (+ 1 counter))
      ;; We need to increment and save
      (with-current-buffer buf
        (erase-buffer)
        (unwind-protect
             (progn
               (insert (format "%d" counter))
               (save-buffer))
          counter)
        counter))))

(provide 'sequence)

;;; sequence.el ends here
