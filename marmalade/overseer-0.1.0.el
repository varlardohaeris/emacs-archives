;;; overseer.el --- Ert-runner Integration Into Emacs.

;; Copyright © 2014 Samuel Tonini
;;
;; Author: Samuel Tonini <tonini.samuel@gmail.com>

;; URL: http://www.github.com/tonini/overseer.el
;; Version: 0.1.0
;; X-Original-Version: 0.1.0
;; Package-Requires: ((emacs "24") (dash "2.10.0") (pkg-info "0.4"))
;; Keywords:

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;  Ert-runner Integration Into Emacs

;;; Code:

(require 'compile)
(require 'dash)
(require 'pkg-info)
(require 'ansi-color)

(defgroup overseer nil
  "Ert-runner Integration Into Emacs."
  :prefix "overseer-"
  :group 'applications
  :link '(url-link :tag "Github" "https://github.com/tonini/overseer.el")
  :link '(emacs-commentary-link :tag "Commentary" "overseer"))

(defvar overseer-command "cask exec ert-runner"
  "The shell command for ert-runner.")

(defvar overseer-buffer-name "*overseer*"
  "Name of the overseer buffer.")

(defun overseer--build-runner-cmdlist (command)
  "Build the commands list for the runner."
  (remove "" (-flatten
              (list (if (stringp command)
                        (split-string command)
                      command)))))

(defvar overseer--project-root-indicators
  '("Cask")
  "list of file-/directory-names which indicate a root of a emacs lisp project")

(defun overseer-project-root ()
  "Return path to the current emacs lisp package root directory."
  (let ((file (file-name-as-directory (expand-file-name default-directory))))
    (overseer--project-root-identifier file overseer--project-root-indicators)))

(defun overseer--project-root-identifier (file indicators)
  (let ((root-dir (if indicators (locate-dominating-file file (car indicators)) nil)))
    (cond (root-dir (directory-file-name (expand-file-name root-dir)))
          (indicators (overseer--project-root-identifier file (cdr indicators)))
          (t nil))))

(defun overseer--establish-root-directory ()
  "Set the default-directory to the emacs lisp package project root."
  (let ((project-root (overseer-project-root)))
    (if (not project-root)
        (error "Couldn't find any emacs lisp project root.")
      (concat project-root "/"))))

(defvar overseer--buffer-name nil
  "Used to store compilation name so recompilation works as expected.")
(make-variable-buffer-local 'overseer--buffer-name)

(defvar overseer--error-link-options
  '(overseer "\\([-A-Za-z0-9./_]+\\):\\([0-9]+\\)\\(: warning\\)?" 1 2 nil (3) 1)
  "File link matcher for `compilation-error-regexp-alist-alist' (matches path/to/file:line).")

(defun overseer--kill-any-orphan-proc ()
  "Ensure any dangling buffer process is killed."
  (let ((orphan-proc (get-buffer-process (buffer-name))))
    (when orphan-proc
      (kill-process orphan-proc))))

(define-compilation-mode overseer-buffer-mode "ert-runner"
  "overseer compilation mode."
  (progn
    (font-lock-add-keywords nil
                            '(("^Finished in .*$" . font-lock-string-face)
                              ("^ert-runner.*$" . font-lock-string-face)))
    ;; Set any bound buffer name buffer-locally
    (setq overseer--buffer-name overseer--buffer-name)
    (set (make-local-variable 'kill-buffer-hook)
         'overseer--kill-any-orphan-proc)))

(defvar overseer--save-buffers-predicate
  (lambda ()
    (not (string= (substring (buffer-name) 0 1) "*"))))

(defun overseer--handle-ansi-color ()
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun overseer-compilation-run (cmdlist buffer-name)
  "Run CMDLIST in `buffer-name'.
Returns the compilation buffer.
Argument BUFFER-NAME for the compilation."
  (save-some-buffers (not compilation-ask-about-save) overseer--save-buffers-predicate)
  (let* ((overseer--buffer-name buffer-name)
         (compilation-filter-start (point-min)))
    (with-current-buffer
        (compilation-start (mapconcat 'concat cmdlist " ")
                           'overseer-buffer-mode
                           (lambda (b) overseer--buffer-name))
      (set (make-local-variable 'compilation-error-regexp-alist-alist)
           (cons overseer--error-link-options compilation-error-regexp-alist-alist)) 
      (set (make-local-variable 'compilation-error-regexp-alist) (cons 'overseer compilation-error-regexp-alist)) 
      (add-hook 'compilation-filter-hook 'overseer--handle-ansi-color nil t))))

(defun overseer--current-buffer-test-file-p ()
  (string-match-p "-test\.el$"
                  (file-name-nondirectory (buffer-file-name))))

(defun overseer-test ()
  "Run ert-runner."
  (interactive)
  (overseer-execute '()))

(defun overseer-help ()
  "Run ert-runner with --help as argument."
  (interactive)
  (overseer-execute '("--help")))

(defun overseer-test-this-buffer ()
  "Run ert-runner with the current `buffer-file-name' as argument."
  (interactive)
  (if (overseer--current-buffer-test-file-p)
      (overseer-execute (list (buffer-file-name)))        
    (message (format "%s is no test file."
                     (file-name-nondirectory (buffer-file-name))))))

(defun overseer-test-debug ()
  "Run ert-runner with --debug as argument."
  (interactive)
  (overseer-execute '("--debug")))

(defun overseer-test-verbose ()
  "Run ert-runner with --verbose as argument."
  (interactive)
  (overseer-execute '("--verbose")))

(defun overseer-test-quiet ()
  "Run ert-runner with --quiet as argument."
  (interactive)
  (overseer-execute '("--quiet")))

(defun overseer-test-prompt (command)
  "Run ert-runner with custom arguments."
  (interactive "Mert-runner: ")
  (message command)
  (overseer-execute (list command)))

(defun overseer-execute (cmdlist)
  "Execute an ert-runner with CMDLIST as arguments."
  (let ((default-directory (overseer--establish-root-directory)))
    (overseer-compilation-run (overseer--build-runner-cmdlist (list overseer-command cmdlist))
                              overseer-buffer-name)))

;;;###autoload
(defun overseer-version (&optional show-version)
  "Get the Overseer version as string.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.

The returned string includes both, the version from package.el
and the library version, if both a present and different.

If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil."
  (interactive (list t))
  (let ((version (pkg-info-version-info 'overseer)))
    (when show-version
      (message "Overseer version: %s" version))
    version))

(defun overseer-mode-hook ()
  "Hook which enables `overseer-mode'"
  (overseer-mode 1))

(defvar overseer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c , t") 'overseer-test)
    (define-key map (kbd "C-c , b") 'overseer-test-this-buffer)
    (define-key map (kbd "C-c , p") 'overseer-test-prompt)
    (define-key map (kbd "C-c , d") 'overseer-test-debug)
    (define-key map (kbd "C-c , q") 'overseer-test-quiet)
    (define-key map (kbd "C-c , v") 'overseer-test-verbose)
    (define-key map (kbd "C-c , h") 'overseer-help)
    map)
  "The keymap used when `overseer-mode' is active.")

;;;###autoload
(define-minor-mode overseer-mode
  "Minor mode for emacs lisp files to test through ert-runner.

Key bindings:
\\{overseer-mode-map}"
  nil
  " overseer"
  :group 'overseer
  :global nil
  :keymap 'overseer-mode-map)

(provide 'overseer)

;;; overseer.el ends here
