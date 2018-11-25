;;; vc-tfs.el --- Support for TFS version control system  -*- lexical-binding:t -*-

;; Copyright (C) 2014 Matthias Meulien

;; Author: Matthias Meulien <orontee@gmail.com>
;; Keywords: vc tools
;; Package: vc
;; URL: https://gitorious.org/vc-tfs-el
;; Version: 0.1.3

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

;; This file contains a VC backend for the TFS version control system.
;;
;; Most of the implementation is taken from vc-svn.el and vc-git.el.
;; Note that this package is rather young and has not been tested a
;; lot, I recommend you set `vc-command-message' to non-nil before
;; use.
;;
;; If checkin messages are encoded, you must customize the variable
;; `vc-tfs-coding-system-for-logs' to the corresponding coding system.

;; Todo:
;;
;; - Add support for rollback
;; - Add support for branches creation/deletion
;; - Add support for labels creation/deletion
;; - Implement change comment modification (`vc-tfs-modify-change-comment')
;; - Add workspace name to `vc-tfs-dir-extra-headers'
;; - Implement `vc-tfs-annotate' and friends
;;
;; Many todos are embedded in the source code.

;; Bugs:
;;
;; - Wrong default-directory when running `vc-print-root-log'
;; - Cursor always at the end of long log buffers
;; - There are missing calls to `vc-set-async-update' to keep views up-to-date

;; Known limitations/Questions:
;;
;; - `diff-hunk-file-names' does not found the file names in TFS diff hunks
;; - Fileset logs and diffs are currently not supported
;; - Should we use the "*vc*" buffer in place of temporary buffers?

;;; Code:

(eval-when-compile
  (require 'vc))

(add-to-list 'vc-handled-backends 'TFS)

;; Clear up the cache to force vc-call to check again and discover
;; new functions when we reload this file.
(put 'TFS 'vc-functions nil)

;;;
;;; Customization options
;;;

(defgroup vc-tfs nil
  "VC Team Foundation Server (TFS) backend."
  :version "24.4"
  :group 'vc)

(defcustom vc-tfs-program "tf"
  "Name of the TFS executable."
  :type 'string
  :group 'vc-tfs)

(defcustom vc-tfs-global-switches nil
  "Global switches to pass to any TFS command.
You may for example specify an argument to authenticate the TFS
user (eg. \"/login:user,password\")."
  :type '(choice (const :tag "None" nil)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List"
			 :value ("")
			 string))
  :version "24.4"
  :group 'vc-tfs)

(defcustom vc-tfs-register-switches nil
  "Switches for registering a file into TFS.
A string or list of strings passed to the checkin program by
\\[vc-register].  If nil, use the value of `vc-register-switches'.
If t, use no switches."
  :type '(choice (const :tag "Unspecified" nil)
		 (const :tag "None" t)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List" :value ("") string))
  :version "24.4"
  :group 'vc-tfs)

(defcustom vc-tfs-diff-switches "/format:Unified"
  "String or list of strings specifying extra switches for TFS diff under VC.
If nil, use the value of `vc-diff-switches' (or `diff-switches'),
together with \"/format:Unified\" to force the use of the UNIX
based 'diff -u' output format. If you want to force an empty list
of arguments, use t."
  :type '(choice (const :tag "Unspecified" nil)
		 (const :tag "None" t)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List" :value ("") string))
  :version "24.4"
  :group 'vc-tfs)

(defcustom vc-tfs-coding-system-for-logs 'cp1252
  "The coding system used to decode logs content."
  :type '(coding-system :tag "Coding system")
  :version "24.4"
  :group 'vc-tfs)

;;; Properties of the backend

(defun vc-tfs-revision-granularity () 'repository)

;;;
;;; State-querying functions
;;;

;;;###autoload (defun vc-tfs-registered (file)
;;;###autoload  "Return non-nil if FILE is registered with TFS."
;;;###autoload  (load "vc-tfs" nil t)
;;;###autoload  (vc-tfs-registered file))

(defun vc-tfs-registered (file)
  "Check whether FILE is registered with TFS."
    (with-temp-buffer
      (cd (file-name-directory file))
      (let* (process-file-side-effects
	     (status
	      (condition-case nil
		  (vc-tfs-command t 0 file "properties")
		(error nil))))
	(eq 0 status))))

;; REMARK Note that localversions miss folders and added files 

(defun vc-tfs-state (file)
  "TFS-specific function to compute FILE version control state."
  (if (not (vc-tfs-registered file))
      'unregistered
    (with-temp-buffer
      (cd (file-name-directory file))
      (vc-tfs-command t 0 file "properties")
      (vc-tfs-parse-properties))))

(defun vc-tfs-working-revision (file)
  "TFS-specific function to compute the working revision of FILE."
    (with-temp-buffer
      (cd (file-name-directory file))
      (vc-tfs-command t 0 file "localversions")
      (goto-char (point-min))
      (cond
       ((re-search-forward (concat
			    (regexp-quote (file-name-nondirectory file))
			    ";C\\([0-9]+\\)")
			   nil t)
	(match-string 1))
       (t "0"))))

(defun vc-tfs-parse-properties ()
  "Parse output of \"tf properties\" command in the current buffer."
  (goto-char (point-min))
  (let ((registered
	 (re-search-forward
	  (concat "Local information:" (string ?\n)
		  "\\(?:.*" (string ?\n) "\\)\\{2\\}"
		  "\\(?:  Changeset +: ?\\([0-9]*\\)\\)" (string ?\n)
		  "\\(?:  Change +: \\([^ ]+\\)\\)" (string ?\n)
		  ".*" (string ?\n)
		  "Server information:" (string ?\n)
		  ".*" (string ?\n)
		  "\\(?:  Changeset +: ?\\([0-9]*\\)\\)" (string ?\n)
		  ".*" (string ?\n)
		  "\\(?:  Lock +: \\([^ ]+\\)\\)" (string ?\n)
		  "\\(?:  Lock owner +: ?\\(.*\\)\\)")
	  nil t)))
    (cond
     (registered
      (let ((change (match-string 2))
	    (obsolete (not (equal (match-string 1) (match-string 3))))
	    (lock (match-string 4))
	    (lock-owner (match-string 5)))
	(cond
	 ((not (equal lock "none")) lock-owner)
	 ((equal change "edit")
	  (if obsolete 'needs-merge 'edited))
	 ((equal change "none")
	  (if obsolete 'needs-update 'up-to-date))
	 ((equal change "add") 'added)
	 ;; TODO Complete with other values of `change'
	 (t (error (concat "Not implemented state: " change))))))
     (t 'unregistered))))

(defun vc-tfs-checkout-model (_files)
   ;; TODO This should be dependent on host properties
  'announce)

;;;
;;; State-changing functions
;;;

(defun vc-tfs-create-repo (_backend)
  "Create a workspace and a workspace mapping."
  ;; (vc-tfs-command "*vc*" 0 name "workspace" '("/new" "/noprompt" "/collection:"))
  ;; (vc-tfs-command "*vc*" 0 nil "workfold" '("/map $/ ." ))
  (error "Not yet implemented"))

(defun vc-tfs-register (files &optional _rev _comment)
  "Register FILES into the TFS version control system."
  (apply 'vc-tfs-command nil 0 files "add" (vc-switches 'TFS 'register)))

(defalias 'vc-tfs-responsible-p 'vc-tfs-registered)

;; TODO We'd better check if file is a descendant of a workspace
;; root

(defun vc-tfs-checkin (files _rev comment)
  "Commit changes in FILES into the TFS version control system."
  (let* ((files (mapcar 'convert-standard-filename files))
	 (args (nconc (list "/comment:\"" comment "\"")
		      (vc-switches 'TFS 'checkin)))
	 (status (apply 'vc-tfs-command nil 1 files "checkin" args)))
    (set-buffer "*vc*")
    (goto-char (point-min))
    (unless (equal status 0)
      ;; TODO Check failure reason to suggest merge
      (error "Check-in failed"))))

(defun vc-tfs-find-revision (file rev buffer)
  "Fetch revision REV of file FILE from the TFS version control system.
If REV is the empty string, fetch the revision of the workspace."
  (let (process-file-side-effects)
    (apply 'vc-tfs-command
	   buffer 0 file "view"
	   (nconc
	    `(,(if (and rev (not (string= rev "")))
		   (concat "/version:C" rev)
		 "/version:W"))
	    (list "/noprompt")))))

(defun vc-tfs-checkout (file &optional _editable rev)
  ;; TODO Don't recreate file if it already exists with required
  ;; version
  (apply 'vc-tfs-command nil 0 file "get"
	   (append
	    (cond
	     ((null rev) (list "/version:W"))
	     ((or (eq rev t) (equal rev "")) "/version:T")
	     (t (list (concat "/version:C" rev))))
	    (list "/noprompt")))
  (vc-tfs-command nil 0 file "checkout" (vc-switches 'TFS 'checkout)))

(defun vc-tfs-revert (file &optional contents-done)
  "Remove pending changes from the workspace for FILE.
Does nothing in case CONTENTS-DONE is non-nil."
  (unless contents-done
    (vc-tfs-command nil 0 file "undo")))

(defun vc-tfs-pull (_prompt)
  "Get last changesets from the TFS version control system."
  (let* ((root default-directory)
	 (buffer (format "*vc-tfs : %s*" (expand-file-name root))))
    (apply 'vc-tfs-command buffer 'async root "get"
	   (append (list "/version:T")
		   (list "/recursive")
		   (list "/noprompt")))))

;;;
;;; History functions
;;;

(defun vc-tfs-print-log (files buffer &optional shortlog start-revision limit)
  "Print commit log associated with FILES into specified BUFFER.
If SHORTLOG is non-nil, detailed entries are printed. Displayed
entries start at START-REVISION if non-nil; Otherwise they start
with the workspace revision. If LIMIT is non-nil, show no more
than this many entries."
  (save-current-buffer
    (vc-setup-buffer buffer)
    (let ((coding-system-for-read vc-tfs-coding-system-for-logs)
	  (inhibit-read-only t)
	  (file (if (listp files)
		    (if (eq (length files) 1)
			(car files)
		      (error "Fileset log is not supported"))
		  files)))
      (goto-char (point-min))
      (apply 'vc-tfs-command buffer 'async (list file) "history"
	     (append
	      (list
	       (if start-revision
		   (format "/version:C%s" start-revision)
		 "/version:W"))
	      (when (not shortlog) (list "/format:detailed"))
	      (when limit (list (format "/stopafter:%s" limit)))
	      (list "/noprompt"))))))

(defun vc-tfs-log-incoming (buffer _remote-location)
  "Insert in BUFFER the revision log for the changes received by a pull."
  (let ((coding-system-for-read vc-tfs-coding-system-for-logs))
    (apply 'vc-tfs-command buffer 0 default-directory "history"
	   (append (list "/version:W~T")
		   (list "/recursive")
		   (list "/noprompt")))))

(defvar log-view-message-re)
(defvar log-view-file-re)
(defvar log-view-font-lock-keywords)
(defvar log-view-per-file-logs)
(defvar log-view-expanded-log-entry-function)

;; Surprisingly, the command "tf history" changes its output format
;; according to the "/recursive" option

(defvar vc-tfs-brief-log-formats
  '(default
     "^\\(?1:[0-9]+\\) +\\(?:[a-z,]+\\) +\\(?2:.\\{17\\}\\) +\\(?3:.\\{10\\}\\) +\\(?4:.+\\)"
     recursive
     "^\\(?1:[0-9]+\\) +\\(?2:.\\{17\\}\\) +\\(?3:.\\{10\\}\\) +\\(?4:.+\\)"))

(defun vc-tfs-guess-brief-log-format ()
  "Return font-lock keywords matching the brief log format.
Searching the buffer to guess the format does not work because
its content can be set asynchronously."
  (let ((type
	 (if (eq vc-log-view-type 'short)
	     'default
	   'recursive)))		; incoming type has the recursive arg
    (cons
     (plist-get vc-tfs-brief-log-formats type)
     '((1 'log-view-message-face)
       (2 'change-log-name)
       (3 'change-log-date)))))

(define-derived-mode vc-tfs-log-view-mode log-view-mode "TFS-Log-View"
  (require 'add-log)
  (let ((brief-log-format
	 (and (not (eq vc-log-view-type 'long))
	      (vc-tfs-guess-brief-log-format))))
    (set (make-local-variable 'log-view-file-re) "\\`a\\`")
    (set (make-local-variable 'log-view-per-file-logs) nil)
    (set (make-local-variable 'log-view-message-re)
	 (if brief-log-format
	     (car brief-log-format)
	   "^Changeset: \\([0-9]+\\)"))
    (when (or (eq vc-log-view-type 'short)
	      (eq vc-log-view-type 'log-incoming))
      (setq truncate-lines t)
      (set (make-local-variable 'log-view-expanded-log-entry-function)
	   'vc-tfs-expanded-log-entry))
    (set (make-local-variable 'log-view-font-lock-keywords)
	 (if brief-log-format
	     (list brief-log-format)
	   (append
	    `((,log-view-message-re (1 'change-log-acknowledgment)))
	    '(("^User: \\(.+\\)" (1 'change-log-name))
	      ("^Date: \\(.+\\)" (1 'change-log-date))))))))

(defun vc-tfs-expanded-log-entry (revision)
  "Return expanded log entry for REVISION."
  (with-temp-buffer
    (let ((coding-system-for-read vc-tfs-coding-system-for-logs))
      (apply 'vc-tfs-command t nil nil "changeset"
	     (append (list revision)
		     (list "/noprompt")))
      (goto-char (point-min))
      (unless (eobp)
	(indent-region (point-min) (point-max) 2)
	(buffer-string)))))

(defun vc-tfs-diff (files &optional oldvers newvers buffer)
  "Insert in BUFFER a difference report between OLDVERS and NEWVERS of FILES.
Current implementation does not work when FILES is a list of many files."
  (let* ((file (if (listp files)
		   (if (eq (length files) 1)
		       (car files)
		     (error "Fileset diffs are not supported"))
		 files))		;TODO Could iterate through files
	 (switches
	  (append
	   (when (file-directory-p file) (list "/recursive"))
	   (if vc-tfs-diff-switches
	       (vc-switches 'TFS 'diff)
	     (nconc (list "/format:Unified")
		    (vc-switches nil 'diff))))))
    (apply 'vc-tfs-command buffer 'async file "diff"
	   (append
	    switches
	    (list
	     (if oldvers
		 (concat "/version:C"
			 (if newvers (concat oldvers "~C" newvers)
			   oldvers))
	       (concat "/version:W" (when newvers "~C" newvers)))))))
  1)

(defun vc-tfs-revision-table (files)
  "Return the list of changeset ids found in history of FILES."
  (let (process-file-side-effects
	table)
    (dolist (file files)
      (with-temp-buffer
	(vc-tfs-command t nil file "history")
	(goto-char (point-min))
	(while (re-search-forward "^\\([0-9]+\\) " nil t)
	  (push (match-string 1) table))))
    table))

(defun vc-tfs-revision-completion-table (files)
  "Return the completion table for FILES revisions.
See `vc-tfs-revision-table'."
  (letrec ((table (lazy-completion-table
		   table (lambda () (vc-tfs-revision-table files)))))
    table))

;;;
;;; Directory functions
;;;

(defun vc-tfs-after-dir-status (callback)
  "Call CALLBACK with list of file status extracted current buffer."
  (let ((state-map '(("add" . added)
		     ("edit" . edited)))	; TODO Find other values
	(re "^\\(.*\\) +\\(edit\\|add\\) +\\(.*\\)$")
	result)
    (goto-char (point-min))
    (while (re-search-forward re nil t)
      (let ((state (cdr (assoc (match-string 2) state-map)))
	    (filename (file-relative-name
		       (replace-regexp-in-string "\\\\" "/" (match-string 3))
		       default-directory)))
	(setq result (cons (list filename state) result))))
    (funcall callback result)))

;; TODO How to list unknown files. Should we use properties?

;; TODO Use the detailed format to prevent troubles with fixed length
;; columns?

(declare-function vc-exec-after "vc-dispatcher" (code))

(defun vc-tfs-dir-status (dir callback)
  "Return a list build from status of files in DIR and calls to CALLBACK."
  (vc-tfs-command (current-buffer) 'async dir "status" "/recursive")
  (vc-run-delayed
    (vc-tfs-after-dir-status callback)))

(defvar vc-tfs-shelve-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'ignore)

    (define-key map [down-mouse-3] 'vc-tfs-shelve-menu)
    (define-key map "\C-k" 'vc-tfs-shelve-delete-at-point)
    (define-key map "U" 'vc-tfs-shelve-unshelve-at-point)
    (define-key map "=" 'vc-tfs-shelve-view-at-point)
    map))

(defvar vc-tfs-shelve-menu-map
  (let ((map (make-sparse-keymap "TFS Shelve")))
    (define-key map [de]
      '(menu-item "Delete shelve" vc-tfs-shelve-delete-at-point
		  :help "Delete the current shelve"))
    (define-key map [un]
      '(menu-item "Unshelve shelve" vc-tfs-shelve-unshelve-at-point
		  :help "Unshelve the current shelve"))
    (define-key map [vi]
      '(menu-item "View shelve" vc-tfs-shelve-view-at-point
		  :help "View the contents of the current shelve"))
    map))

(defun vc-tfs-dir-extra-headers (_dir)
  "Return the string to add to the *vc-dir* buffer header.
The string is made of the enumeration of shelves."
  (let ((shelveset (vc-tfs-shelve-list))
	(shelve-help-echo "Use M-x vc-tfs-shelve to create a shelve."))
    (if shelveset
	(concat
	 (propertize "Shelves    :\n" 'face 'font-lock-type-face
		     'help-echo shelve-help-echo)
	 (mapconcat
	  (lambda (x)
	    (propertize x
			'face 'font-lock-variable-name-face
			'mouse-face 'highlight
			'help-echo "mouse-3: Show shelve menu"
			'keymap vc-tfs-shelve-map))
	  shelveset "\n"))
      (concat
       (propertize "Shelves      : " 'face 'font-lock-type-face
		   'help-echo shelve-help-echo)
       (propertize "No shelve"
		   'help-echo shelve-help-echo
		   'face 'font-lock-variable-name-face)))))

;;;
;;; Miscellaneous
;;;

(defun vc-tfs-root (file)
  "Return the root of the hierarchy for FILE."
  (with-temp-buffer
    (let ((dir
	   (if (file-directory-p file)
	       file
	     (file-name-directory file))))
      (cd dir)
      (vc-tfs-command t 0 nil "workfold")
      (goto-char (point-min))
      (when (re-search-forward " \\$.+: \\(.*\\)")
	(match-string 1)))))

(defun vc-tfs-previous-revision (_file rev)
  "Return the revision preceding REV."
  (let ((newrev (1- (string-to-number rev))))
    (when (< 0 newrev)
      (number-to-string newrev))))

(defun vc-tfs-next-revision (file rev)
  "Return FILE's revision following REV.
The returned revision is always lesser than FILE workspace
revision."
  (let ((newrev (1+ (string-to-number rev))))
    (unless (< (string-to-number (vc-tfs-working-revision file))
	       newrev)
      (number-to-string newrev))))

(defun vc-tfs-delete-file (file)
  (vc-tfs-command nil 0 file "delete"))

(defun vc-tfs-rename-file (old new)
  (vc-tfs-command nil 0 new "rename" (file-relative-name old)))

;;;
;;; Shelve functions
;;;

(defun vc-tfs-shelve-list ()
  (with-temp-buffer
    (vc-tfs-command t nil nil "shelvesets")
    (goto-char (point-min))
    (forward-line 2)
    (let ((re "^\\([^ ]+\\) ")		; FIXME Don't work when shelve
					; name contain whitespaces
	  result)
      (while (re-search-forward re nil t)
	(setq result
	      (cons (concat "             " (match-string 1)) result)))
      result)))

(defun vc-tfs-shelve (name)
  "Shelve the pending changes."
  (interactive "sShelve name: ")
  (when (not (equal name ""))
    (vc-tfs-command "*vc-tfs-shelve*" 'async nil "shelve" name)
    (vc-dir-refresh)))

(defun vc-tfs-shelve-view (name)
  "View the content of shelve NAME."
  (interactive "sShelve name: ")
  (vc-setup-buffer "*vc-tfs-shelve*")
  (vc-tfs-command "*vc-tfs-shelve*" 'async nil "diff"
		  "/noprompt" (concat "/shelveset:" name))
  (set-buffer "*vc-tfs-shelve*")
  (diff-mode)
  (setq buffer-read-only t)
  (pop-to-buffer (current-buffer)))

(defun vc-tfs-shelve-unshelve (name)
  "Unshelve shelve NAME."
  (interactive "sUnshelve: ")
  (when (not (equal name ""))
    (vc-tfs-command "*vc-tfs-shelve*" 'async nil "unshelve" "/noprompt" name)))

(defun vc-tfs-shelve-get-at-point (point)
  (save-excursion
    (goto-char point)
    (beginning-of-line)
    (if (looking-at "^ +\\(.+\\)")
	(match-string 1)
      (error "Cannot find shelve at point"))))

(defun vc-tfs-shelve-delete-at-point ()
  (interactive)
  (let ((name (vc-tfs-shelve-get-at-point (point))))
    (when (y-or-n-p (format "Delete shelve %s ? " name))
      (vc-tfs-command "*vc-tfs-shelve*" 'async nil "shelve"
		      "/delete" "/noprompt" name)
      (vc-dir-refresh))))

(defun vc-tfs-shelve-view-at-point ()
  (interactive)
  (vc-tfs-shelve-view (vc-tfs-shelve-get-at-point (point))))

(defun vc-tfs-shelve-unshelve-at-point ()
  (interactive)
  (vc-tfs-shelve-unshelve (vc-tfs-shelve-get-at-point (point))))

(defun vc-tfs-shelve-menu (e)
  (interactive "e")
  (vc-dir-at-event e (popup-menu vc-tfs-shelve-menu-map e)))

;;;
;;; Internal functions
;;;

(defun vc-tfs-command (buffer okstatus file-or-list &rest flags)
  "A wrapper around `vc-do-command' for use in vc-tfs.el.
The difference to vc-do-command is that this function always invokes `tfs',
and that it passes `vc-tfs-global-switches' to it before FLAGS."
  (apply 'vc-do-command (or buffer "*vc*") okstatus vc-tfs-program file-or-list
         (if (stringp vc-tfs-global-switches)
             (cons vc-tfs-global-switches flags)
           (append vc-tfs-global-switches flags))))

(provide 'vc-tfs)

;;; vc-tfs.el ends here
