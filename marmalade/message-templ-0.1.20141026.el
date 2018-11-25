;;; message-templ.el --- Templates for message-mode.

;; Copyright (C) 1998,1999,2000 Masahiro MURATA <muse@ba2.so-net.ne.jp>
;; Copyright (C) 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>
;; Copyright (C) 2004 ARISAWA Akihiro <ari@mbf.sphere.ne.jp>

;; Author: ARISAWA Akihiro <ari@mbf.sphere.ne.jp>
;; Keywords: mail, news, template
;; Version: 0.1.20141026
;; Maintainer: David Bremner <david@tethera.net>
;; URL: git://pivot.cs.unb.ca/message-templ.git

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;

;;; Commentary:

;; This package is a port of the Wanderlust template facility to
;; message-mode, the message composition mode used by e.g. Gnus,
;; notmuch-emacs, and mu4e.

;;; Code:

(require 'message)
(autoload 'article-display-x-face "gnus-art" nil t)

(defgroup message-temp nil
  "Template for message composing."
  :group 'message)

(defcustom message-templ-alist nil
  "Alist of template.
First element of each list is a string specifies the name of the template.
Remaining elements indicate actions."
  :type '(repeat (list (string :tag "Name")
		       (repeat
			:inline t
			(choice (cons (sexp :tag "Field(Variable)")
				      (sexp :tag "Value"))
				(sexp :tag "Function")))))
  :group 'message-templ)

(defcustom message-templ-visible-select t
  "*If non-nil, select template with visible."
  :type 'boolean
  :group 'message-templ)

(defcustom message-templ-confirm nil
  "*If non-nil, require your confirmation when selected template."
  :type 'boolean
  :group 'message-templ)

(defcustom message-templ-buffer-lines 7
  "*Lines of template buffer."
  :type 'integer
  :group 'message-templ)

(defvar message-templ-default-name "default")
(defvar message-templ-buffer-name "*Message-Template*")
(defvar message-templ-mode-map nil)

(defvar message-templ nil)
(defvar message-templ-cur-num 0)
(defvar message-templ-max-num 0)
(defvar message-templ-draft-buffer nil)
(defvar message-templ-preview nil)
(defvar message-templ-config-variables nil)

(defvar message-templ-config-sub-function-alist
  '((body		. message-templ-config-sub-body)
    (top		. message-templ-config-sub-top)
    (bottom		. message-templ-config-sub-bottom)
    (header		. message-templ-config-sub-header)
    (header-top		. message-templ-config-sub-header-top)
    (header-bottom	. message-templ-config-sub-header)
;    (part-top		. message-templ-config-sub-part-top)
;    (part-bottom	. message-templ-config-sub-part-bottom)
    (body-file		. message-templ-config-sub-body-file)
    (top-file		. message-templ-config-sub-top-file)
    (bottom-file	. message-templ-config-sub-bottom-file)
    (header-file	. message-templ-config-sub-header-file)
    (template		. message-templ-config-sub-template)
    (x-face		. message-templ-config-sub-x-face)))

(unless message-templ-mode-map
  (setq message-templ-mode-map (make-sparse-keymap))
  (define-key message-templ-mode-map "p" 'message-templ-prev)
  (define-key message-templ-mode-map "n" 'message-templ-next)
  (define-key message-templ-mode-map "q" 'message-templ-abort)
  (define-key message-templ-mode-map "\r" 'message-templ-set)
  (define-key message-templ-mode-map "\n" 'message-templ-set))

(defsubst message-templ-config-sub-eval-insert (content &optional newline)
  (let (content-value)
    (when (and content
	       (stringp (setq content-value (eval content))))
      (insert content-value)
      (when newline (insert "\n")))))

(defun message-templ-config-sub-body (content)
  (message-goto-body)
  (delete-region (point) (point-max))
  (message-templ-config-sub-eval-insert content))

(defun message-templ-config-sub-top (content)
  (message-goto-body)
  (message-templ-config-sub-eval-insert content))

(defun message-templ-config-sub-bottom (content)
  (goto-char (point-max))
  (message-templ-config-sub-eval-insert content))

(defun message-templ-config-sub-header (content)
  (message-goto-eoh)
  (message-templ-config-sub-eval-insert content 'newline))

(defun message-templ-config-sub-header-top (content)
  (goto-char (point-min))
  (message-templ-config-sub-eval-insert content 'newline))

;(defun message-templ-config-sub-part-top (content)
;  (goto-char (mime-edit-content-beginning))
;  (message-templ-config-sub-eval-insert content 'newline))

;(defun message-templ-config-sub-part-bottom (content)
;  (goto-char (mime-edit-content-end))
;  (message-templ-config-sub-eval-insert content 'newline))

(defsubst message-templ-config-sub-file (content)
  (let ((coding-system-for-read 'undecided)
	(file (expand-file-name (eval content))))
    (if (file-exists-p file)
	(insert-file-contents file)
      (error "%s: no exists file" file))))

(defun message-templ-config-sub-body-file (content)
  (message-goto-body)
  (delete-region (point) (point-max))
  (message-templ-config-sub-file content))

(defun message-templ-config-sub-top-file (content)
  (message-goto-body)
  (message-templ-config-sub-file content))

(defun message-templ-config-sub-bottom-file (content)
  (goto-char (point-max))
  (message-templ-config-sub-file content))

(defun message-templ-config-sub-header-file (content)
  (message-goto-eoh)
  (message-templ-config-sub-file content))

(defun message-templ-config-sub-template (content)
  (setq message-templ-config-variables
	(message-templ-insert (eval content))))

(defun message-templ-config-sub-x-face (content)
  (save-restriction
    (message-narrow-to-headers)
    (message-remove-header "X-Face"))
  (message-position-on-field "X-Face" "From")
  (nnheader-insert-file-contents content))

(defun message-templ-config-sub-function (field content)
  (let (func)
    (when (setq func (assq field message-templ-config-sub-function-alist))
      (let (message-templ-config-variables)
	(funcall (cdr func) content)
	;; for message-templ-config-sub-template
	(cons t message-templ-config-variables)))))

(defun messsage-templ-config-exec-sub (clist)
  (let (config local-variables)
    (while clist
      (setq config (car clist))
      (cond
       ((functionp config)
	(funcall config))
       ((consp config)
	(let ((field (car config))
	      (content (cdr config))
	      ret-val)
	  (cond
	   ((stringp field)
	    (save-restriction
	      (message-narrow-to-headers)
	      (message-remove-header field))
	    (message-position-on-field field)
	    (insert (eval content)))
	   ((setq ret-val (message-templ-config-sub-function field content))
	    (when (cdr ret-val) ;; for message-templ-config-sub-template
	      (setq local-variables (nconc local-variables (cdr ret-val)))))
	   ((boundp field) ;; variable
	    (make-local-variable field)
	    (set field (eval content))
	    (setq local-variables (nconc local-variables (list field))))
	   (t
	    (error "%s: not variable" field)))))
       (t
	(error "%s: not supported type" config)))
      (setq clist (cdr clist)))
    local-variables))

(defun message-templ-preview-p ()
  "Return non-nil when preview template."
  message-templ-preview)

(defun message-templ-apply (name)
  "Apply NAME template to draft."
  (let (template)
    (when name
      (when (string= name "")
	(setq name message-templ-default-name))
      (when (setq template (cdr (assoc name message-templ-alist)))
	(save-excursion
	  (messsage-templ-config-exec-sub template))))))

(defun message-templ-mode ()
  "Major mode for message template.

\\{message-templ-mode}

Entering Message-Templ mode calls the value of `message-templ-mode-hook'."
  (kill-all-local-variables)
  (setq mode-name "Message-Templ"
	major-mode 'message-templ-mode)
  (use-local-map message-templ-mode-map)
  (set (make-local-variable 'font-lock-defaults)
       '(message-font-lock-keywords t))
  (setq buffer-read-only t)
  (run-hooks 'message-templ-mode-hook))

;;;###autoload
(defun message-templ-select (&optional arg)
  "Select template from `message-templ-alist'."
  (interactive "P")
  (if (not (if arg
	       (not message-templ-visible-select)
	     message-templ-visible-select))
      (message-templ-apply
       (completing-read (format "Template (%s): " message-templ-default-name)
			message-templ-alist))
    (let* ((begin message-templ-default-name)
	   (work message-templ-alist))
      (when (and begin (cdr (assoc begin message-templ-alist)))
	(while (not (string= (car (car work)) begin))
	  (setq message-templ-cur-num (1+ message-templ-cur-num))
	  (setq work (cdr work))))
      (setq message-templ nil
	    message-templ-cur-num 0
	    message-templ-max-num (length message-templ-alist))
      (setq message-templ-draft-buffer (current-buffer))
      (if (get-buffer-window message-templ-buffer-name)
	  (select-window (get-buffer-window message-templ-buffer-name))
	(let* ((cur-win (selected-window))
	       (size (min
		      (- (window-height cur-win)
			 window-min-height 1)
		      (- (window-height cur-win)
			 (max window-min-height
			      (1+ message-templ-buffer-lines))))))
	  (split-window cur-win (if (> size 0) size window-min-height))
	  ;; goto the bottom of the two...
	  (select-window (next-window))
	  ;; make it display...
	  (let ((pop-up-windows nil))
	    (switch-to-buffer (get-buffer-create message-templ-buffer-name)))))
      (set-buffer message-templ-buffer-name)
      (message-templ-mode)
      (message-templ-show))))

(defun message-templ-show (&optional arg)
  "Show reference INDEX in `message-templ-alist'.
vARG is ignored."			; ARG ignored this version (?)
  (save-excursion
    (set-buffer message-templ-buffer-name)
    (let ((buffer-read-only nil)
	  (message-templ-preview t)
	  (mail-header-separator  "--header separater--"))
      (erase-buffer)
      (goto-char (point-min))
      (message-templ-insert
       (setq message-templ
	     (car (nth message-templ-cur-num message-templ-alist)))
       mail-header-separator)
      (let ((gnus-article-buffer (current-buffer)))
	(article-display-x-face))
      (setq mode-line-process (concat ":" message-templ))
      (set-buffer-modified-p nil))))

(defun message-templ-next ()
  "Display next reference in other buffer."
  (interactive)
  (when (= message-templ-max-num
	   (setq message-templ-cur-num (1+ message-templ-cur-num)))
    (setq message-templ-cur-num 0))
  (message-templ-show))

(defun message-templ-prev ()
  "Display previous reference in other buffer."
  (interactive)
  (setq message-templ-cur-num (if (zerop message-templ-cur-num)
				  (1- message-templ-max-num)
				(1- message-templ-cur-num)))
  (message-templ-show))

(defun message-templ-abort ()
  "Exit from electric reference mode without inserting reference."
  (interactive)
  (setq message-templ nil)
  (delete-window)
  (kill-buffer message-templ-buffer-name)
  (when (buffer-live-p message-templ-draft-buffer)
    (set-buffer message-templ-draft-buffer)
    (let ((win (get-buffer-window message-templ-draft-buffer)))
      (when win (select-window win)))))

(defun message-templ-set ()
  "Exit from electric reference mode and insert selected reference."
  (interactive)
  (if (and message-templ-confirm
	   (not (y-or-n-p "Are you sure ? ")))
      (message "")
    (delete-window)
    (kill-buffer message-templ-buffer-name)
    (when (buffer-live-p message-templ-draft-buffer)
      (set-buffer message-templ-draft-buffer)
      (message-templ-apply message-templ)
      (let ((win (get-buffer-window message-templ-draft-buffer)))
	(when win (select-window win))))))

(defun message-templ-insert (name &optional mail-header)
  "Insert NAME template.
Set header-separator is MAIL-HEADER."
  (let ((template (cdr (assoc name message-templ-alist)))
	(mail-header-separator (or mail-header
				   mail-header-separator)))
    (when template
      (when mail-header
	(insert mail-header-separator "\n"))
      (messsage-templ-config-exec-sub template))))

(defun message-templ-config-exec (&optional config-alist)
  "Change headers according to the value of `message-templ-config-alist'."
  (interactive)
  (let ((case-fold-search t)
	(alist (or config-alist message-templ-config-alist))
	local-variables key clist found)
    (save-excursion
      (while alist
	(setq key (caar alist)
	      clist (cdar alist))
	(cond
	 ((stringp key)
	  (if (save-restriction
		  (message-narrow-to-headers)
		  (goto-char (point-min))
		  (re-search-forward key nil t))
	      (message-templ-config-exec-sub clist)))
	 ((eval key)
	  (message-templ-config-exec-sub clist)))
	(setq alist (cdr alist))))))

(provide 'message-templ)
;;; message-templ.el ends here
