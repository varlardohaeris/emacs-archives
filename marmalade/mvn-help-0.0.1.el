;;; mvn-help.el --- maven help tools

;; Copyright (C) 2013  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: languages, processes
;; Created: 28th March 2013
;; Version: 0.0.1

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

;; Some stuff to help with the Maven build tool for Java.

;;; Code:

;;;###autoload
(defun mvn-buffer-init ()
  "Initialize a buffer.

Use this as a hook function in java-mode."
  (make-variable-buffer-local 'compile-command)
  (setq compile-command
        (format
         "cd %s ; mvn test"
         (locate-dominating-file (buffer-file-name) "pom.xml"))))

;;;###autoload
(defun mvn-init ()
  "Initialize this package."
  (interactive)
  (add-hook 'java-mode-hook 'mvn-buffer-init))

(provide 'mvn-help)

;;; mvn-help.el ends here
