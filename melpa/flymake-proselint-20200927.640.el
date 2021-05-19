;;; flymake-proselint.el --- Flymake backend for proselint -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Manuel Uberti <manuel.uberti@inventati.org>
;;
;; Author: Manuel Uberti <manuel.uberti@inventati.org>
;; Version: 0.1.0
;; Package-Version: 20200927.640
;; Package-Commit: b94950301139846002d2020bc630440ff834bf24
;; Keywords: convenience
;; Package-Requires: ((emacs "26.1") (flymake-quickdef "1.0.0"))
;; URL: https://github.com/manuel-uberti/flymake-proselint

;; flymake-proselint is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later version.
;;
;; flymake-proselint is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License
;; along with flymake-proselint.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; This package adds support for proselint (http://proselint.com/) in Flymake.

;; Once installed, the backend can be enabled with:
;; (add-hook 'markdown-mode-hook #'flymake-proselint-setup)

;;; Code:

(require 'flymake)
(require 'flymake-quickdef)

(flymake-quickdef-backend
  flymake-proselint-backend
  :pre-let ((proselint-exec (executable-find "proselint")))
  :pre-check (unless proselint-exec (user-error "Executable proselint not found on PATH"))
  :write-type 'pipe
  :proc-form (list proselint-exec "-")
  :search-regexp "^.+:\\([[:digit:]]+\\):\\([[:digit:]]+\\): \\(.+\\)$"
  :prep-diagnostic (let* ((lnum (string-to-number (match-string 1)))
                          (lcol (string-to-number (match-string 2)))
                          (msg (match-string 3))
                          (pos (flymake-diag-region fmqd-source lnum lcol))
                          (beg (car pos))
                          (end (cdr pos)))
                     (list fmqd-source beg end :warning msg)))

;;;###autoload
(defun flymake-proselint-setup ()
  "Enable Flymake backend proselint."
  (add-hook 'flymake-diagnostic-functions #'flymake-proselint-backend nil t))

(provide 'flymake-proselint)

;;; flymake-proselint.el ends here
