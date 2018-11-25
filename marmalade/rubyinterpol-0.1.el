;;; rubyinterpol.el --- Ruby-like String Interpolation for format

;;; -*- lexical-binding: t -*-

;; Copyright (C) 2013  Jaime Fournier <jaimef@linbsd.org>

;; Author: Jaime Fournier <jaimef@linbsd.org>
;; Keywords: Ruby-like String Interpolation
;; Version: 0.1

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
;; This allows you to use the following syntax
;; (ris "This is a test of variable #{foo} and #{bar}")
;; Equivalent to (format "This is a test of variable %s and %s" foo bar)
;;

;; Many thanks to freenode #emacs

;;; Code:

(defun ris-vars (str)
  (let ((result nil))
    (with-temp-buffer
      (insert str)
      (goto-char
       (point-min))
      (while (re-search-forward "#{\\([^}]*\\)}" nil t)
        (setq result
              (cons
               (intern (match-string 1))
               result)))
      (reverse result))))

(defun ris-format (str)
  (replace-regexp-in-string "#{.[^#]*}" "%s" str))

(defun ris (str)
  "Ruby like interposition for strings"
  (apply #'format (ris-format str) (mapcar #'symbol-value (ris-vars str))))

(provide 'rubyinterpol)

;;; rubyinterpol.el ends here
