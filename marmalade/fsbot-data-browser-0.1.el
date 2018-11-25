;;; fsbot-data-browser.el --- browse the fsbot database using tabulated-list-mode
;;
;; Filename: fsbot-data-browser.el
;; Description: Browse the fsbot database using tabulated-list-mode
;; Author: Benaiah Mischenko
;; Maintainer: Benaiah Mischenko
;; Created: Thu September 15 2016
;; Version: 0.1
;; Package-Requires: ((dash "2.12.1"))
;; Last-Updated: Thu September 15 2016
;;           By: Benaiah Mischenko
;;     Update #: 1
;; URL: http://github.com/benaiah/fsbot-data-browser
;; Doc URL: http://github.com/benaiah/fsbot-data-browser
;; Keywords: fsbot, irc, tabulated-list-mode
;; Compatibility: GNU Emacs: 24.x, 25.x
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This is a simple mode that lets you download and view the fsbot
;; database in tabulated-list-mode.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'dash)

;;;###autoload
(defun fsbot-download-data ()
  "Download fsbot db for viewing. You must do this before you can use `fsbot-view-data'."
  (interactive)
  (url-retrieve
   "http://gnufans.net/~fsbot/data/botbbdb"
   (lambda (&rest _)
     (write-region nil nil "~/.emacs.d/.fsbot-data-raw" nil)
     (message "fsbot data finished downloading"))))

(defun fsbot-slurp-file-into-buffer (filename)
  (insert-file-contents filename)
  (buffer-substring-no-properties (point-min) (point-max)))

(defun fsbot-parse-data ()
  (goto-char 0)
  (let ((ret '()))
    (while (search-forward "[" nil t)
      (push (read-from-string (thing-at-point 'line t)) ret))
    ret))

(defun fsbot-process-notes (notes)
  (if notes
      (let ((real-notes (car (read-from-string notes))))
        (cond ((not real-notes) "nil")
              ((stringp real-notes) real-notes)
              ((and (listp real-notes)
                    (= 1 (length real-notes))
                    (stringp (car real-notes)))
               (car real-notes))
              (t (format "%S" real-notes))))
    "nil"))

(defun fsbot-load-data ()
  (let ((fsbot-parsed-data
         (with-temp-buffer (fsbot-slurp-file-into-buffer "~/.emacs.d/.fsbot-data-raw")
                           (fsbot-parse-data))))
    (-map (lambda (entry)
            (let ((key (aref (car entry) 0))
                  (notes (fsbot-process-notes
                          (cdr (car (cdr (aref (car entry) 7)))))))
              `(,key [,key ,notes])))
          fsbot-parsed-data)))

;;;###autoload
(define-derived-mode fsbot-data-browser-mode tabulated-list-mode
  "Fsbot Data Browser" "Tabulated-list-mode browser for fsbot data."
  (setq tabulated-list-format [("Key" 30 t)
                               ("Notes" 0 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Key" nil))
  (tabulated-list-init-header))

;;;###autoload
(defun fsbot-list-data (data)
  (pop-to-buffer "*fsbot data*" nil)
  (fsbot-data-browser-mode)
  (setq truncate-lines t)
  (setq tabulated-list-entries data)
  (tabulated-list-print t))

(defun fsbot-view-data ()
  "View fsbot db. You must call `fsbot-download-data' before this will work."
  (interactive)
  (fsbot-list-data (fsbot-load-data)))

(provide 'fsbot-data-browser)

;;; fsbot-data-browser.el ends here
