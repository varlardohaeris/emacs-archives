;;; gitattributes-whitespace.el --- configure whitespace settings from gitattributes

;; Copyright (C) 2014 Peter Eisentraut

;; Author: Peter Eisentraut <peter@eisentraut.org>
;; URL: https://github.com/petere/emacs-gitattributes-whitespace
;; Version: 1.20141128
;; Keywords: convenience, tools, vc

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This library sets `indent-tabs-mode', `tab-width', as well as
;; `whitespace-style' (from `whitespace-mode') according to the
;; settings of the gitattributes(5) whitespace setting for the file
;; (as best as they can be matched).  If the file is not in a Git
;; repository, or there are no gitattributes set, then nothing
;; happens.
;;
;; `gitattributes-whitespace-apply' can be called interactively, but
;; is best called via `find-file-hook' (ideally last).  This is set up
;; automatically if this library is installed as a package.

;;; Code:

(defun gitattributes-check (attribute)
  "Read gitattribute ATTRIBUTE of current buffer file."
  (let* ((out (shell-command-to-string (concat "git check-attr " attribute " " (buffer-file-name))))
         (result (nth 2 (split-string out " "))))
    (if (string= result "unspecified\n")
        nil
      result)))

;;;###autoload
(defun gitattributes-whitespace-apply ()
  "Apply gitattribute whitespace settings to current buffer."
  (interactive)
  (if (and vc-mode
           (boundp 'vc-git-program))
      (let ((check-attr (gitattributes-check "whitespace")))
        (if check-attr
            (dolist (value (split-string check-attr ","))
              (when (or (string= value "blank-at-eol")
                        (string= value "trailing-space"))
                (when (boundp 'whitespace-style)
                  (add-to-list 'whitespace-style 'trailing)))
              (when (or (string= value "blank-at-eof")
                        (string= value "trailing-space"))
                (when (boundp 'whitespace-style)
                  (add-to-list 'whitespace-style 'empty)))
              (when (string= value "space-before-tab")
                (when (boundp 'whitespace-style)
                  (add-to-list 'whitespace-style 'space-before-tab::tab)))
              (when (string= value "indent-with-non-tab")
                (setq indent-tabs-mode t)
                (when (boundp 'whitespace-style)
                  (add-to-list 'whitespace-style 'indentation)))
              (when (string= value "tab-in-indent")
                (setq indent-tabs-mode nil)
                (when (boundp 'whitespace-style)
                  (add-to-list 'whitespace-style 'indentation)))
              (when (string= (substring value 0 9) "tabwidth=")
                (setq tab-width (string-to-number (substring value 9)))))))))

;;;###autoload
(add-hook 'find-file-hook 'gitattributes-whitespace-apply t)

(provide 'gitattributes-whitespace)

;;; gitattributes-whitespace.el ends here
