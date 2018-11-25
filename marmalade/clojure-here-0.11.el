;;; clojure-here.el --- Clojure process in a buffer
;;
;; License: GPL
;; Copyright (C) 2013, Jude Chao, all rights reserved.
;; Author: Jude Chao
;; Email: <kaihaosw@gmail.com>
;; URL: https://github.com/judevc
;; Version: 0.11
;; Keywords: clojure
;;
;; Based on cmuscheme.el, scheme-here.el and inf-clojure.el
;;
;;
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

(provide 'clojure-here)
(require 'clojure-mode)
(require 'comint)

(defvar clojure-program-name "lein repl")

(defun clojure-here-buffer ()
  (format "*clojure: %s*" default-directory))

(defun clojure-here-proc ()
  (let ((proc (get-buffer-process (if (eq major-mode 'inferior-clojure-mode)
                                      (current-buffer)
                                    (clojure-here-buffer)))))
    (or proc (error "No clojure process here"))))

(defun run-clojure-here ()
  "Run a new clojure process at the directory of the current buffer.
   If a process is already running, switch to its buffer"
  (interactive)
  (let* ((proc (format "clojure: %s" default-directory))
         (buf (format "*%s*" proc)))
    (unless (comint-check-proc buf)
      (let ((cmd (split-string clojure-program-name)))
        (set-buffer
         (apply 'make-comint-in-buffer proc buf (car cmd) nil (cdr cmd)))
        (inferior-clojure-mode)
        (buffer-resize)))
    (pop-to-buffer buf)))

(defun buffer-resize ()
  (delete-other-windows)
  (split-window-vertically (floor (* 0.68 (window-height))))
  (other-window 1)
  (switch-to-buffer buf)
  (other-window 1))

(defun inferior-clojure-mode ()
  "Major mode for interacting with an inferior clojure process."
  (interactive)
  (comint-mode)
  ;; Customise in inferior-clojure-mode-hook
  (setq comint-prompt-regexp "^[^>\n]*>+ *") ; OK for clojure, oaklisp, T,...
  (setq major-mode 'inferior-clojure-mode)
  (setq mode-name "Inferior Clojure")
  (setq mode-line-process '(": %s"))
  (setq comint-input-filter (function clojure-input-filter))
  (setq comint-get-old-input (function clojure-get-old-input))
  (run-hooks 'inferior-clojure-mode-hook))

(defun clojure-input-filter (str)
  "Don't save anything matching inferior-clojure-filter-regexp"
  (not (string-match inferior-clojure-filter-regexp str)))

(defvar inferior-clojure-filter-regexp "\\`\\s *\\S ?\\S ?\\s *\\'"
  "*Input matching this regexp are not saved on the history list.
Defaults to a regexp ignoring all inputs of 0, 1, or 2 letters.")

(defun clojure-get-old-input ()
  "Snarf the sexp ending at point"
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (buffer-substring (point) end))))

(defun switch-to-clojure-here ()
  "Switch to the buffer-local clojure process."
  (interactive)
  (let ((buf (clojure-here-buffer)))
    (if (get-buffer buf)
        (pop-to-buffer buf)
      (error "No clojure process here"))
    (push-mark)))

(defun clojure-here-send-region (start end)
  "Send the current region to the buffer-local clojure process."
  (interactive "r")
  (let ((proc (clojure-here-proc)))
    (comint-send-region proc start end)
    (comint-send-string proc "\n")))

(defun clojure-here-send-sexp ()
  "Send the last sexp to the buffer-local clojure process."
  (interactive)
  (clojure-here-send-region (save-excursion (backward-sexp) (point)) (point)))

(defun clojure-here-send-def ()
  "Send the current definition to the buffer-local clojure process."
  (interactive)
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (clojure-here-send-region (point) end))))

(defun clojure-here-send-region/switch (start end)
  "Send the current region to the buffer-local clojure process.
   Then switch to its buffer."
  (interactive "r")
  (clojure-here-send-region start end)
  (switch-to-clojure-here))

(defun clojure-here-send-sexp/switch ()
  "Send the last sexp to the buffer-local clojure process.
   Then switch to its buffer."
  (interactive)
  (clojure-here-send-sexp)
  (switch-to-clojure-here))

(defun clojure-here-send-def/switch ()
  "Send the current definition to the buffer-local clojure process.
   Then swith to its buffer."
  (interactive)
  (clojure-here-send-def)
  (switch-to-clojure-here))

(defvar inferior-clojure-mode-hook nil
  "This hook is run when cmuclojure is loaded in.
   This is a good place to put keybindings.")

(run-hooks 'inferior-clojure-mode-hook)

(add-hook 'clojure-mode-hook
          (lambda ()
            (local-unset-key (kbd "C-c C-z"))
            (local-unset-key (kbd "C-M-x"))
            (local-unset-key (kbd "C-x C-e"))
            (local-unset-key (kbd "C-c C-e"))
            (local-unset-key (kbd "C-c C-r"))
            (define-key clojure-mode-map (kbd "C-c M-j") 'run-clojure-here)
            (define-key clojure-mode-map (kbd "C-x e") 'clojure-here-send-sexp)
            (define-key clojure-mode-map (kbd "C-c r") 'clojure-here-send-region)
            (define-key clojure-mode-map (kbd "C-c e") 'clojure-here-send-def)
            (define-key clojure-mode-map (kbd "C-x C-e") 'clojure-here-send-sexp/switch)
            (define-key clojure-mode-map (kbd "C-c C-r") 'clojure-here-send-region/switch)
            (define-key clojure-mode-map (kbd "C-c C-e") 'clojure-here-send-def/switch)
            (define-key clojure-mode-map (kbd "C-c z") 'switch-to-clojure-here)))

;;; clojure-here.el ends here
