;;; how-many-lines-in-project.el --- Calculate how many lines are there in your project.

;; Copyright (C) 2014 Wei Zhao
;; Author: Wei Zhao <kaihaosw@gmail.com>
;; Git: https://github.com/kaihaosw/how-many-lines-in-project.git
;; Version: 0.3
;; Created: 2014-07-24
;; Keywords: project, convenience
;; Package-Requires:((find-file-in-project "3.3"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
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

;;; Commentary:

;; This library provides a method for quickly calculating how many
;; lines in a given project.
;; It requires `find-file-in-project'.

;; Usage
;; (require 'how-many-lines-in-project)
;; M-x: how-many-lines-in-project

;; You may need to config the variable `ffip-patterns' in `find-file-in-project'.
;; For example:
;; (eval-after-load 'find-file-in-project
;;   (progn
;;     (setq ffip-patterns (append '("*.scala" "*.sbt") ffip-patterns))
;;     (setq ffip-patterns (append '("*.scm" "*.ss") ffip-patterns))))
;; or
;; (eval-after-load 'how-many-lines-in-project
;;   (progn
;;     (setq ffip-patterns (append '("*.scala" "*.sbt") ffip-patterns))
;;     (setq ffip-patterns (append '("*.scm" "*.ss") ffip-patterns))))

;;; Code:

(require 'find-file-in-project)

(defvar how-many-lines-in-project-buffer-name
  "*hm-lines*")

(defun how-many-lines-in-project-list ()
  "Return a list which contains file-line and file-name."
  (mapcar (lambda (l) (cons (string-to-number
                        (replace-regexp-in-string
                         "\n" "" (shell-command-to-string
                                  (concat "wc -l " (cdr l) " | awk '{print $1}'")))) l))
          (ffip-project-files)))

;;;###autoload
(defun how-many-lines-in-project ()
  "Calculate how many lines are there in your project."
  (interactive)
  (let* ((fll (how-many-lines-in-project-list))
         (name-length (apply 'max (mapcar (lambda (x) (length x)) (mapcar 'cadr fll))))
         (total-lines (apply '+ (mapcar 'car fll))))
    (get-buffer-create how-many-lines-in-project-buffer-name)
    (switch-to-buffer how-many-lines-in-project-buffer-name)
    (setq buffer-read-only nil)
    (erase-buffer)
    (mapcar (lambda (x)
              (insert
               (format
                (format "%%-%ds %%%ds lines\n" name-length 8)
                (cadr x) (car x))))
            fll)
    (insert "\n-----------------------\n")
    (insert (format "total %d lines\n" total-lines))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (switch-to-buffer how-many-lines-in-project-buffer-name)))

(provide 'how-many-lines-in-project)

;;; how-many-lines-in-project.el ends here

