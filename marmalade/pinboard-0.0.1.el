;;; pinboard.el --- get stuff from pinboard -*- lexical-binding: t -*-

;; Copyright (C) 2013  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: multimedia
;; Created: 10th March 2013
;; Version: 0.0.1
;; Created: 10th March 2013
;; Url: https://gist.github.com/nicferrier/5043465

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

;; http://pinboard.in is an awesome tool for bookmarking with lots of
;; extensions. This Emacs extension let's you pull the data into a
;; creole-mode file presented in a creole like way.

;; I use it for making quick blog posts of stuff I've pinboarded.

;; I expect there are other uses that I'll add.

;;; Code:

(require 'web)
(require 'json)
(require 'creole-mode)

(defgroup pinboard nil
  "Pull data from pinboard.in."
  :group 'applications)

(defcustom pinboard-url
  (format "http://feeds.pinboard.in/json/u:%s/?count=10" user-login-name)
  "Your pinboard.in user"
  :group 'pinboard)

(defun pinboard/data (data)
  "The data is a vector of alists."
  (with-current-buffer (get-buffer-create "*pinboard-latest*")
    (let ((buffer-read-only nil))
      (erase-buffer)
      (loop for al across data
         do
           (progn
             (setf (car (assoc 't al)) 'tags)
             (destructuring-bind (&key tags a dt n d u)
                 (kvalist->plist al)
               (insert
                (format "=== %s ===
[[%s|%s]]
//%s//
%s
%s\n\n" n u n dt d (mapconcat 'identity tags "|"))))))
      (creole-mode)
      (visual-line-mode)
      (goto-char (point-min))
      (view-mode)
      (switch-to-buffer (current-buffer)))))

;;;###autoload
(defun pinboard-latest ()
  (interactive)
  (let ((buf (get-buffer-create "*pinboard-data*")))
    (with-current-buffer buf (erase-buffer))
    (web-http-get
     (lambda (htp headers data)
       (let ((jsdata
              (with-current-buffer buf
                (goto-char (point-max))
                (insert data)
                (goto-char (point-min))
                (json-read))))
         (pinboard/data jsdata)))
     :url pinboard-url)))

(provide 'pinboard)

;;; pinboard.el ends here
