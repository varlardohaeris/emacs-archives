;;; cmuclojure.el --- Clojure process in a buffer

;; License: GPL
;; Copyright (C) 2013, Jude Chao, all rights reserved.
;; Author: Jude Chao
;; Email: <kaihaosw@gmail.com>
;; URL: https://github.com/judevc
;; Version: 0.2
;; Keywords: clojure

;; Based on cmuscheme.el and https://github.com/syohex/emacs-inf-clojure/blob/master/inf-clojure.el

;;; Commentary:
;; Run clojure in your buffer, and send your definition to your buffer immediately.
;; Install: (require 'cmuclojure)
;; Keybinding: C-c M-j run-clojure
;;             C-c e clojure-send-definition
;;             C-c C-e clojure-send-definition-and-go
;;             C-c r clojure-send-region
;;
;;; License
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

(provide 'cmuclojure)
(require 'clojure-mode)
(require 'comint)

(defvar inferior-clojure-mode-hook nil
  "*Hook for customising inferior-clojure mode.")
(defvar inferior-clojure-mode-map nil)

(define-key clojure-mode-map (kbd "C-c M-j") 'run-clojure)
(define-key clojure-mode-map (kbd "C-c e") 'clojure-send-definition)
(define-key clojure-mode-map (kbd "C-c C-e") 'clojure-send-definition-and-go)
(define-key clojure-mode-map (kbd "C-c r") 'clojure-send-region)
;; (define-key clojure-mode-map (kbd "C-c M-r") 'clojure-send-region-and-go)
;; (define-key clojure-mode-map (kbd "C-c C-z") 'switch-to-clojure)

(defun inferior-clojure-mode ()
  "Major mode for interacting with an inferior clojure process."
  (interactive)
  (comint-mode)
  ;; Customise in inferior-clojure-mode-hook
  (setq comint-prompt-regexp "^[^>\n]*>+ *") ; OK for clojure, oaklisp, T,...
  ;; (clojure-mode-variables)
  (setq major-mode 'inferior-clojure-mode)
  (setq mode-name "Inferior Clojure")
  (setq mode-line-process '(": %s"))
  ;; (use-local-map inferior-clojure-mode-map)
  (setq comint-input-filter (function clojure-input-filter))
  ;; (setq comint-input-sentinel (function ignore))
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

;;; Obsolete
(defvar clojure-program-name "lein repl")
(defvar clojure-repl-name "*clojure*")

(defun run-clojure (cmd)
  (clojure-split-window)
  (interactive (list (if current-prefix-arg
                         (read-string "Run clojure: " clojure-program-name)
                       clojure-program-name)))
  (if (not (comint-check-proc "*clojure*"))
      (let ((cmdlist (split-string-and-unquote cmd)))
        (set-buffer (apply 'make-comint "clojure" (car cmdlist)
                           (clojure-start-file (car cmdlist)) (cdr cmdlist)))
        (inferior-clojure-mode)))
  (setq clojure-program-name cmd)
  (setq clojure-buffer "*clojure*")
  (pop-to-buffer "*clojure*"))

(defun clojure-start-file (prog)
  (let* ((progname (file-name-nondirectory prog))
         (start-file (concat "~/.emacs_" progname))
         (alt-start-file (concat user-emacs-directory "init_" progname ".clj")))
    (if (file-exists-p start-file)
        start-file
      (and (file-exists-p alt-start-file) alt-start-file))))

(defvar clojure-buffer nil "*The current clojure process buffer.")

(defun clojure-proc ()
  (unless (and clojure-buffer
               (get-buffer clojure-buffer)
               (comint-check-proc clojure-buffer))
    (clojure-interactively-start-process))
  (or (clojure-get-process)
      (error "No current process.  See variable `clojure-buffer'")))

(defun clojure-get-process ()
  "Return the current Clojure process or nil if none is running."
  (get-buffer-process (if (eq major-mode 'inferior-clojure-mode)
                          (current-buffer)
                        clojure-buffer)))

(defun clojure-interactively-start-process (&optional cmd)
  "Start an inferior clojure process.  Return the process started.
Since this command is run implicitly, always ask the user for the
command to run."
  (save-window-excursion
    (run-clojure (read-string "Run Clojure: " clojure-program-name))))

(defun clojure-split-window ()
  (cond
   ((= 1 (count-windows))
    (delete-other-windows)
    (split-window-vertically (floor (* 0.68 (window-height))))
    (other-window 1)
    (switch-to-buffer "*clojure*")
    (other-window 1))
   ((not (find "*clojure*"
               (mapcar (lambda (w) (buffer-name (window-buffer w)))
                       (window-list))
               :test 'equal))
    (other-window 1)
    (switch-to-buffer "*clojure*")
    (other-window -1))))

(defun switch-to-clojure (eob-p)
  (interactive "P")
  (if (or (and clojure-buffer (get-buffer clojure-buffer))
          (clojure-interactively-start-process))
      (pop-to-buffer clojure-buffer)
    (error "No current process buffer.  See variable `clojure-buffer'"))
  (when eob-p
    (push-mark)
    (goto-char (point-max))))

(defun clojure-send-region (start end)
  "Send the current region to the inferior Clojure process."
  (interactive "r")
  (clojure-split-window)
  (comint-send-region (clojure-proc) start end)
  (comint-send-string (clojure-proc) "\n"))

(defun clojure-send-region-and-go ()
  (interactive)
  (clojure-send-region)
  (switch-to-clojure t))

(defun clojure-send-definition ()
  "Send the current definition to the inferior Clojure process."
  (interactive)
  (clojure-split-window)
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (clojure-send-region (point) end))))

(defun clojure-send-definition-and-go ()
  (interactive)
  (clojure-send-definition)
  (switch-to-clojure 5))

;; (defun clojure-send-definition-split-window ()
;;   (interactive)
;;   (clojure-split-window)
;;   (clojure-send-definition))

(defvar clojure-source-modes '(clojure-mode))

(defvar cmusclojure-load-hook nil
  "This hook is run when cmuscheme is loaded in.
This is a good place to put keybindings.")

(run-hooks 'cmuclojure-load-hook)

;;; cmuclojure.el ends here
