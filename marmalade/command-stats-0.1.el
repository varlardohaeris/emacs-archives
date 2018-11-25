;;; command-stats.el --- Track frequency of commands executed in emacs

;; Copyright (c) 2013 Thejaswi Puthraya

;; Author: Thejaswi Puthraya <thejaswi.puthraya@gmail.com>
;; Created: 03 May 2013
;; Version: 0.1
;; Keywords: command-history command frequency

;; This file is not part of GNU Emacs.

;; This file is distributed under the MIT License
;; Copyright (c) 2013 Thejaswi Puthraya

;; Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; Use this emacs exit hook to keep a track of your most used emacs
;; commands and then bind them to some comfortable keys (like F5-F9)
;; and become more productive because every 'key' counts!

;; Usage:

;; In your init file, load this elisp file:
;; (require 'command-stats)

;; To customize the variables:
;; (custom-set-variables
;;   (commands-save-directory "~/emacs_commands")
;;   (commands-save-file "commands.el"))

;;; Change Log:

;; 0.1 - Initial commit and push into marmalade

;; Source is available at https://github.com/theju/command-stats.el

;;; Code:
(defgroup command-stats nil
  "Maintain frequency of all executed commands"
  :group 'applications)

(defcustom commands-save-directory "~/.emacs.d/commands"
  "The directory where the commands summary is stored"
  :type 'string
  :group 'command-stats)

(defcustom commands-save-file "commands.el"
  "The file under the commands-save-directory
where the commands statistics are stored"
  :type 'string
  :group 'command-stats)

;; Set the history-length variable to -1 to prevent
;; command truncation
(setq history-length -1)

(defun save-file-hook ()
  (let ((commands-save-file-path (concat (file-name-as-directory
					  commands-save-directory)
					 commands-save-file))
	(commands (mapcar (lambda (x) (car x)) command-history))
	(commands-assoc ()))
    (unless (file-exists-p commands-save-directory)
      (make-directory commands-save-directory t))
    (if (file-exists-p commands-save-file-path)
	(with-current-buffer (find-file commands-save-file-path)
	  (setq commands-assoc
		(read
		 (buffer-substring-no-properties (point-min) (point-max))))
	  (erase-buffer)))
    (dolist (command commands)
      (if (assoc command commands-assoc)
	  (incf (cdr (assoc command commands-assoc)))
        (setq commands-assoc (append commands-assoc `((,command . 1))))))
    (setq commands-assoc (sort commands-assoc (lambda (x y)
						(> (cdr x) (cdr y)))))
    (with-current-buffer (find-file commands-save-file-path)
      (insert (pp-to-string commands-assoc))
      (save-buffer)
      (kill-buffer))))

(add-hook 'kill-emacs-hook 'save-file-hook)

(provide 'command-stats)

;;; command-stats.el ends here
