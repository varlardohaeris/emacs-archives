;;; lisp-infection.el --- Commands to *enhance* S-exp editing
;;
;; Author: Eric Crosson
;; URL: https://github.com/EricCrosson/lisp-infection
;; Version: 0.0.10
;;
;; General enhancements to the lisp production process
;;
;; Alhough I'm not a fan of modiying the global key map, here is an
;; example snippet of lisp to assign key bindings to the useful
;; commands:
;;
;; (global-set-key (kbd "C-M-s-n") 'name-of-current-defun)
;; (global-set-key (kbd "C-c l \\") 'indent-entire-defun)
;; (global-set-key (kbd "C-s-e") 'eval-current-defun)
;; (global-set-key (kbd "C-c l e") 'eval-current-sexp)
;; (global-set-key (kbd "C-c l i") 'surround-current-sexp)
;; (global-set-key (kbd "C-c l d") 'dissolve-current-sexp)
;; (global-set-key (kbd "C-c l j") 'create-sexp-on-new-line)
;; (global-set-key (kbd "C-c l k") 'hatch-current-sexp)
;; (global-set-key (kbd "C-S-s-k") 'copy-sexp)


(defun surround-current-sexp ()
  "Surround the current sexp with parens and a new command.
Can be invoked from inside the sexp to surround, or at the opening
paren of the sexp to surround."
  (interactive)
  (save-excursion
    (let ((new-sexp (concat (read-string "New sexp: ") " ")))
      (when (not (looking-at "("))
	(backward-up-list))
      (let ((old-sexp (buffer-substring (point) (progn (forward-sexp) (point)))))
	(backward-sexp)
	(delete-sexp)
	(insert "()")
	(forward-char -1)
	(insert new-sexp)
	(insert old-sexp)))))

(defun hatch-current-sexp ()
  "Bring a sexp out of its shell and destroy the surrounding sexp."
  (interactive)
  (backward-up-list)
  (let ((old-sexp (buffer-substring (point) (progn (forward-sexp) (point)))))
    (backward-up-list)
    (insert old-sexp)
    (delete-sexp)
    (backward-sexp)
    (forward-char 1)))

(defun dissolve-current-sexp ()
  "Delete the current sexp, transferring arguments to the prevoius
sexp at point. If the current sexp contains no arguments, it deletes
the entire sexp including parens."
  (interactive)
  (when (not (looking-at "("))
    (backward-up-list))
  (re-search-forward "[ )]" nil t)
  (cond ((char-equal ?\) (char-before))
	 (backward-sexp)
	 (delete-sexp))
	((char-equal ?\s (char-before))
	 (let ((sexp-args (buffer-substring (point) (end-of-sexp))))
	   (backward-up-list)
	   (delete-sexp)
	   (save-excursion
	     (insert sexp-args)
	     (delete-char 1))))))

(defun mark-current-defun ()
  "Mark the current defun."
  (interactive)
  (beginning-of-defun)
  (push-mark)
  (end-of-defun))

(defun mark-current-sexp ()
  "Mark the current sexp."
  (interactive)
  (backward-up-list)
  (push-mark)
  (forward-sexp))

(defun copy-sexp ()
  "Save a sexp to the kill ring without killing it."
  (interactive)
  (kill-ring-save (beginning-of-sexp) (end-of-sexp))
  (message "Sexp copied. %s %s" (beginning-of-sexp) (end-of-sexp)))

(defun end-of-sexp ()
  "Returns the end of the current sexp."
  (save-excursion
    (when (not (looking-at ")"))
      (backward-up-list)
      (forward-sexp))
    (point)))

(defun beginning-of-sexp ()
  "Returns the beginning of the current sexp."
  (save-excursion
    (when (not (looking-at "("))
      (backward-up-list))
    (point)))

(defun delete-sexp ()
  "Delete sexp without saving to the kill ring."
  (interactive)
  (save-excursion
    (let ((begin (progn (when (not (looking-at "("))
			  (backward-up-list))
			(point)))
	  (end (progn (forward-sexp) (point))))
      (delete-region begin end))))

(defun create-sexp-on-new-line ()
  "Create a new sexp at this point on a new line."
  (interactive)
  (newline-and-indent)
  (insert "()")
  (forward-char -1))

(defun eval-current-sexp ()
  "Evaluate the current sexp."
  (interactive)
  (save-excursion
    (when (not (char-equal (char-before) ?\)))
      (when (not (looking-at "("))
	(backward-up-list))
      (forward-sexp))
    (call-interactively 'eval-last-sexp)))

(defun indent-entire-defun ()
  "Re-indent the entire defun for ease of editing."
  (interactive)
  (save-excursion
    (let ((begin (progn (beginning-of-defun) (point)))
	  (end (progn (end-of-defun) (point))))
      (indent-region begin end)
      (message "Indented %s." (name-of-current-defun)))))

(defun eval-current-defun ()
  "Re-evalutae the current defun."
  (interactive)
  (save-excursion
    (end-of-defun)
    (call-interactively 'eval-last-sexp)
    (message "Evaluated %s." (name-of-current-defun))))

(defun name-of-current-defun ()
  "Return the name of the current defun.
If called interactively, the name will be printed in the minibuffer.
If called from emacs-lisp code, the name will be returned as a string."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (forward-word)
    (forward-char)
    (if (called-interactively-p 'any)
	(message (current-word))
      (current-word))))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;;; lisp-infection.el ends here
