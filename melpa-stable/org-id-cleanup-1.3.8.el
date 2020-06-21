;;; org-id-cleanup.el --- Interactively clean up unused IDs of org-id     -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Free Software Foundation, Inc.

;; Author: Marc Ihm <1@2484.de>
;; URL: https://github.com/marcIhm/org-id-cleanup
;; Package-Version: 1.3.8
;; Package-Commit: 05d57840893d9ae8146ed41d29d1f0571c05ded8
;; Version: 1.3.8
;; Package-Requires: ((org "9.2.6") (dash "2.12.0") (emacs "25.1"))

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by

;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;

;;; Commentary:

;; Purpose:
;;
;;  Interactively clean up unused IDs of org-id.
;;  The term 'unused' refers to IDs, that have been created by org-id
;;  regularly, but are now no longer referenced from anywhere within in org.
;;  This might e.g. happen by deleting a link, that once referenced such an id.
;;
;;  Normal usage of org-id does not lead to a lot of such unused IDs, and
;;  org-id does not suffer much from them.
;;
;;  However, some usage patterns or packages (like org-working-set) may
;;  produce a larger number of such unused IDs; in such cases it might be
;;  helpful to clean up with org-id-cleanup.
;;
;; Setup:
;;
;;  org-id-cleanup should be installed with package.el
;;

;;; Change Log:

;;   Version 1.3
;;
;;   - Write a log of deleted IDs
;;
;;   Version 1.2
;;
;;   - Adding tests
;;   - Preparing for melpa
;;
;;   Version 1.1
;;
;;   - Respecting attachments
;;   - Refactoring
;;
;;   Version 1.0
;;
;;   - Initial Version
;;

;;; Code:

(require 'org)
(require 'button)
(require 'org-attach)
(require 'dash)
(require 'subr-x)
(require 'org-id)

;; Version of this package
(defvar org-id-cleanup-version "1.3.8" "Version of `org-working-set', format is major.minor.bugfix, where \"major\" are incompatible changes and \"minor\" are new features.")

(defvar org-id-cleanup--all-steps '(backup save complete-files review-files collect-ids review-ids cleanup-ids save-again) "List of all supported steps.")
(defvar org-id-cleanup--initial-files nil "List of files to be scanned while cleaning ids without user added files.")
(defvar org-id-cleanup--all-files nil "List of all files to be scanned while cleaning ids.")
(defvar org-id-cleanup--unref-unattach-ids nil "List of IDs not referenced from files.  not having attachments.")
(defvar org-id-cleanup--num-deleted-ids 0 "Number of IDs deleted.")
(defvar org-id-cleanup--num-attach 0 "Number of IDs that are referenced by their attachment directory only.")
(defvar org-id-cleanup--num-all-ids 0 "Number of all IDs.")
(defvar org-id-cleanup--log-buffer-name "Log of IDs deleted by org-id-cleanup" "Name of buffer where log changes will be written.")
(defvar org-id-cleanup--log-file-name (concat (file-name-directory org-id-locations-file) "org-id-cleanup-log-of-deletions.org")
 "Filename for log buffer; derived from value of 'org-id-locations-file'.")
(defvar org-id-cleanup--log-buffer nil "Log buffer, once opened.")

;; User-visible function and dispatch
(defun org-id-cleanup ()
  ;; Do NOT edit the part of this help-text before version number. It will
  ;; be overwritten with Commentary-section from beginning of this file.
  ;; Editing after version number is fine.
  ;;
  ;; For Rake: Insert purpose here
  "Interactively clean up unused IDs of org-id.
The term 'unused' refers to IDs, that have been created by org-id
regularly, but are now no longer referenced from anywhere within in org.
This might e.g. happen by deleting a link, that once referenced such an id.

Normal usage of org-id does not lead to a lot of such unused IDs, and
org-id does not suffer much from them.

However, some usage patterns or packages (like org-working-set) may
produce a larger number of such unused IDs; in such cases it might be
helpful to clean up with org-id-cleanup.

This is version 1.3.8 of org-id-cleanup.el.

This assistant is the only interactive function of this package.
Detailed explanations are shown in each step; please read them
carefully and then operate the relevant buttons."
  (interactive)
  (org-id-cleanup--do nil 'backup))


(defun org-id-cleanup--do (come-from go-to)
  "Do the work for `org-id-cleanup'.
Argument COME-FROM is previous step or nil, GO-TO the next step or one of
symbols 'previous or 'next."
  (let (step)

    ;; check arguments and compute step
    (if (and come-from
             (not (member come-from org-id-cleanup--all-steps)))
        (error "Internal error with come-from: %s" come-from))
    (unless (if come-from
                (member go-to '(previous next))
              (member go-to org-id-cleanup--all-steps))
      (error "Internal error with go-to: %s" go-to))
    (setq step
          (if come-from
              (nth (+ (if (eq go-to 'next) +1 -1)
                      (org-id-cleanup--step-to-num come-from)) org-id-cleanup--all-steps)
            go-to))
    (unless step (error "Internal error with step, come-from, go-to: %s/%s/%s" step come-from go-to))
    
    ;; prepare buffer
    (pop-to-buffer-same-window (get-buffer-create "*Assistant for deleting IDs*"))
    (setq buffer-read-only nil)
    (delete-other-windows)
    (erase-buffer)
    ;; breadcrumbs
    (let ((in-past-steps t))
      (dolist (st org-id-cleanup--all-steps)
        (insert (propertize (format "%s - " (symbol-name st)) 'face (if in-past-steps nil 'org-agenda-dimmed-todo-face)))
        (if (eq st step) (setq in-past-steps nil))))
    (backward-delete-char 3)
    
    (insert "\n\nThis assistant helps to clean up IDs from your org-files, it tries to remove only IDs, that are not referenced any longer.\n\n")

    ;; common controls
    (when (eq step 'backup)
      (insert "It operates in steps, and explains what is going to happen in each step; it presents buttons, that when pressed execute the described action and take you to the next step. Pressing a button can be done either with the return-key or with the mouse.")
      (fill-paragraph)
      (insert "\n\n")
      (insert "The line of steps at the top of this window shows the progress within this assistant. No IDs will be deleted unless you confirm so in step 'cleanup-ids'")
      (fill-paragraph)
      (insert "\n\n"))
    (insert (format "Step %d of %s: %s"
                    (1+ (org-id-cleanup--step-to-num step))
                    (length org-id-cleanup--all-steps)
                    (symbol-name step)))
    (when (> (org-id-cleanup--step-to-num step) 0)
      (insert "   (or back to ")
      (insert-button
       (symbol-name (nth (1- (org-id-cleanup--step-to-num step)) org-id-cleanup--all-steps)) 'action
       (lambda (_) (org-id-cleanup--do step 'previous)))
      (insert ")"))
    (insert "\n\n")
    
    ;; prepare list of files for some steps
    (setq org-id-cleanup--initial-files
          (org-id-cleanup--normalize-files
	   ;; Agenda files and all associated archives
	   (org-agenda-files t org-id-search-archives)
	   ;; Explicit extra files
	   (unless (symbolp org-id-extra-files)
	     org-id-extra-files)
	   ;; All files known to have IDs
	   org-id-files
           ;; some lisp-files that may contain IDs
           (list user-init-file
                 custom-file)))

    ;; dispatch according to step
    ;; next step will be bound to button within each previous step, so no logic here
    (cond

     ((eq step 'backup)
      (org-id-cleanup--step-backup step))

     ((eq step 'save)
      (org-id-cleanup--step-save step))

     ((eq step 'complete-files)
      (org-id-cleanup--step-complete-files step org-id-cleanup--initial-files))

     ((eq step 'review-files)
      (org-id-cleanup--step-review-files step org-id-cleanup--all-files))

     ((eq step 'collect-ids)
      (org-id-cleanup--step-collect-ids step org-id-cleanup--all-files))

     ((eq step 'review-ids)
      (org-id-cleanup--step-review-ids step))
     
     ((eq step 'cleanup-ids)
      (org-id-cleanup--step-cleanup-ids step org-id-cleanup--unref-unattach-ids))

     ((eq step 'save-again)
      (org-id-cleanup--step-save-again))

     (t
      (error "Step %s not supported" step)))

    ;; finish buffer before leaving it to the user to press any buttons therein; see individual steps
    (recenter -1)
    (message "Please read comments and instructions and proceed by clicking the appropriate buttons.")
    (setq buffer-read-only t)))


;; Individual steps
(defun org-id-cleanup--step-backup (this-step)
  "Step from `org-id-cleanup--do'.
Argument THIS-STEP contains name of current step."

  (if (not org-id-track-globally)
      (insert "\n\nThe variable `org-id-track-globally' is not set, therefore this assistant cannot be useful and will not continue.\n")

    (insert "\nPlease make sure that you have a backup, if something goes wrong !\n\nThis assistant cannot do this for you; so please come back when done\nand press this ")
    (insert-button "button" 'action
                     (lambda (_) (org-id-cleanup--do this-step 'next)))))


(defun org-id-cleanup--step-save (this-step)
  "Step from `org-id-cleanup--do'.
Argument THIS-STEP contains name of current step."
  
  (insert "You need to save all org buffers and update org-id locations: ")

  (insert-button
   "go" 'action
   (lambda (_)
     (goto-char (point-max))
     (setq buffer-read-only nil)
     (insert "\n\nSaving buffers ... ")
     (redisplay)
     (org-save-all-org-buffers)
     (insert "done\nUpdating id locations ... ")
     (redisplay)
     (org-id-update-id-locations)
     ;; continue with next step
     (org-id-cleanup--do this-step 'next))))


(defun org-id-cleanup--step-complete-files (this-step files)
  "Step from `org-id--cleanup-do.
Argument THIS-STEP contains name of current step, FILES is list of files to present to user for completion."
  (let ((head-of-files "--- start of extra files ---"))
    (insert (format "Complete the list of %d files that will be scanned and might be changed:\n\n" (length files)))
    (org-id-cleanup--insert-files files)

    (insert "\n\nPlease make sure, that this list is complete, i.e. includes all files that:\n\n"
            " - Contain nodes with IDs            (which will be removed if not referenced)\n"
            " - Have references or links to IDs   (which protect those IDs from being removed)\n\n"
            "(of course, most of your org-files may contain both)")
    (insert "\n\nPlease note: If the list above is incomplete regarding the second aspect,\nthis will probably lead to IDs being removed, that are still referenced\nfrom a file missing in the list.")
    (fill-paragraph)

    (insert "\n\nIDs may also appear in lisp-files, so your user init file has already been added. But if you use IDs from within other lisp-code, this will not be noticed. However, to protect such IDs once and for all, it is enough to list them anywhere within your org-files.")
    (fill-paragraph)

    (insert "\n\nFinally, you might have the habit of using IDs completely outside of org (e.g. in your calendar); such use cannot be noticed by this package, and if there are no other references from within org, such IDs will be deleted. But again, to protect these IDs, it is enough to list them anywhere within your org-files.")
    (fill-paragraph)

    (insert "\n\nTo add files or directories to this list and only for this assistant, please ")
    (insert-button
     "browse" 'action
     (lambda (_)
       (let ((file (read-file-name "Choose a single files or a whole directory: " org-directory)))
         (when file
           (setq buffer-read-only nil)
           (goto-char (point-max))
           (search-backward head-of-files)
           (end-of-line)
           (insert "\n" file)
           (setq buffer-read-only t)))))
    (insert "\n\n" head-of-files "\n")
    (insert "---  end  of extra files ---\n")
    (insert "\nAfter doing that, you might want to ")

    (insert-button
     "continue" 'action
     (lambda (_)
       ;; change global state
       (setq org-id-cleanup--all-files
             (org-id-cleanup--normalize-files
              files
              (org-id-cleanup--collect-extra-files head-of-files)))
       ;; continue with next step
       (org-id-cleanup--do this-step 'next)))))


(defun org-id-cleanup--step-review-files (this-step files)
  "Step from `org-id--cleanup-do.
Argument THIS-STEP contains name of current step, FILES is the list of files to review."
  (insert (format "Review the list of %d files that will be scanned; the org-file among them might be changed:\n\n" (length files)))
  (org-id-cleanup--insert-files files)
  (insert "\n\nWhen satisfied ")

  (insert-button
   "continue" 'action
   (lambda (_)
     ;; continue with next step
     (org-id-cleanup--do this-step 'next))))
     

(defun org-id-cleanup--step-collect-ids (this-step files)
  "Step from `org-id--cleanup-do.
Argument THIS-STEP contains name of current step, FILES is the list of files with IDs."
  (let ((counters (make-hash-table :test 'equal))
        (scanned 0)
        (attach 0)
        pgreporter unref unref-unattach)
    (insert (format "Now the relevant %d files will be scanned for IDs.\n\n" (length files)))
    (insert "Any IDs, that are used for attachment directories will be kept; the same is true,\nif the node is merely tagged as having an attachment.\n\n")
    (insert "From now on, please refrain from leaving this assistant to create links to IDs, because they would not be taken into account any more.")
    (fill-paragraph)
    (insert "\n\nScan files for IDs and ")

    (insert-button
     "continue" 'action
     (lambda (_)
       (let (ids)
         (maphash (lambda (id _) (push id ids)) org-id-locations)
         (setq pgreporter (make-progress-reporter (format "Scanning %d files..." (length files)) 1 (length files)))
         (dolist (file files)
           (with-current-buffer (find-file-noselect file)
             (dolist (id ids)
               (goto-char (point-min))
               (while (search-forward id nil t)
                 (cl-incf (gethash id counters 0)))))
           (progress-reporter-update pgreporter (cl-incf scanned)))

         ;; keep only IDs, that have appeared only once
         (maphash (lambda (id count) (if (eq count 1) (push id unref))) counters)

         ;; keep only IDs, that are not used in attachment dir
         (dolist (id unref)
           (let ((pos (org-id-find id)))
             (with-current-buffer  (find-file-noselect (car pos))
               (goto-char (cdr pos))
               ;; assume id is used in attachments even if only last 12 chars match
               (if (or (string= (org-attach-dir-from-id id) (org-attach-dir))
                       (cl-search (substring id -12) (org-attach-dir))
                       (member "ATTACH" (org-get-tags))
                       (member "attach" (org-get-tags))
                       (member org-attach-auto-tag (org-get-tags)))
                   (cl-incf attach)
                 (push id unref-unattach)))))

         (progress-reporter-done pgreporter)

         ;; change global state
         (setq org-id-cleanup--unref-unattach-ids unref-unattach)
         (setq org-id-cleanup--num-all-ids (length ids))
         (setq org-id-cleanup--num-attach attach)
         ;; continue with next step
         (org-id-cleanup--do this-step 'next))))))


(defun org-id-cleanup--step-review-ids (this-step)
  "Step from `org-id--cleanup-do'.
Argument THIS-STEP contains name of current step."
  (let ((head-of-ids "--- List of IDs to be deleted ---")
        pt pt2 pct)
    (insert (format "Find below the list of IDs (%d out of %d) that will be deleted; pressing TAB on an id will show the respective node.\n" (length org-id-cleanup--unref-unattach-ids) org-id-cleanup--num-all-ids))
    (insert (format "%d IDs are not in the list and will be kept, because they have associated attachments.\n\n" org-id-cleanup--num-attach))
    (insert "You may remove IDs from the list as you like to keep them from being deleted.\nUsual editing commands (e.g. C-k) apply.")
    (setq pct (* 100 (/ (float (length org-id-cleanup--unref-unattach-ids)) org-id-cleanup--num-all-ids)))
    (when (< pct 20)
      (insert (format "\n\nThere are %d IDs to be deleted among total %d. This is a percentage of %.1f %% only. By deleting them you will not notice much of a difference and you may well skip the rest of this assistant altogether. However deletion does no harm either, epecially if you do this as part of a regular maintainance." (length org-id-cleanup--unref-unattach-ids) org-id-cleanup--num-all-ids pct))
      (fill-paragraph))
    (insert "\n\nIf satisfied ")

    (insert-button
     "continue" 'action
     (lambda (_)
       (local-unset-key (kbd "<tab>")) ; tab is no longer needed in next step
       ;; change global state
       (setq org-id-cleanup--unref-unattach-ids (org-id-cleanup--collect-ids head-of-ids))
       ;; continue with next step
       (org-id-cleanup--do this-step 'next)))

    (setq pt (point))
    (insert "\n\n(deletion will not happen yet.)")
    (insert "\n\n\n" head-of-ids "\n")
    (setq pt2 (point))
    (dolist (id org-id-cleanup--unref-unattach-ids)
      (insert id "\n"))
    (add-text-properties pt2 (point) '(inhibit-read-only t))
    (goto-char pt)
    (local-set-key (kbd "<tab>") 'org-id-cleanup--peek-into-id)))


(defun org-id-cleanup--step-cleanup-ids (this-step ids)
  "Step from `org-id--cleanup-do.
Argument THIS-STEP contains name of current step, IDS given ids to remove."

  (let ((scanned 0)
        pgreporter pt)
    (insert "Please make sure, that you have not manually created new links referencing any IDs while the last two steps of this assistant were active.")
    (fill-paragraph)
    (insert
     (format "\n\nFor your reference, a log of all changes will be appended to %s.\n" org-id-cleanup--log-file-name)
     "This log will contain sufficient information (id, filename, point and outline path) to manually restore selected IDs later; you may browse it before saving your files in the last step.")
    (fill-paragraph)
    (insert (propertize (format "\n\n\n  >>>  To REMOVE %s IDs out of %d UNCONDITIONALLY, press this " (length org-id-cleanup--unref-unattach-ids) org-id-cleanup--num-all-ids) 'face 'org-warning))
    
    (insert-button
     "button" 'action
     (lambda (_)
       (org-id-cleanup--open-log (length org-id-cleanup--unref-unattach-ids) org-id-cleanup--num-all-ids)
       (setq buffer-read-only nil)
       (goto-char (point-max))
       (setq org-id-cleanup--num-deleted-ids 0)
       (insert "\n\nRemoving unused IDs ... ")
       (redisplay)
       (setq pgreporter (make-progress-reporter (format "Removing %d IDs..." (length org-id-cleanup--unref-unattach-ids)) 1 (length org-id-cleanup--unref-unattach-ids)))

       (dolist (id ids)
         (pop-to-buffer (find-file-noselect (gethash id org-id-locations)))
         (goto-char (point-min))
         (search-forward id)
         (unless (string= id (org-id-get))
           (error "Expected id of this node to be %s, but found %s" id (org-id-get)))
         (org-id-cleanup--append-to-log id (buffer-file-name) (point) (-concat (org-get-outline-path) (list (nth 4 (org-heading-components)))))
         (org-delete-property "ID")
         (cl-incf org-id-cleanup--num-deleted-ids)
         (progress-reporter-update pgreporter (cl-incf scanned)))

       (progress-reporter-done pgreporter)
       (org-id-cleanup--write-log)
       (sleep-for 1)

       ;; change global state
       (setq org-id-cleanup--unref-unattach-ids nil)

       ;; continue with next step
       (org-id-cleanup--do this-step 'next)))
    (insert (propertize "  <<<" 'face 'org-warning))
    (setq pt (point))
    (insert "\n\n\nOr, to review those IDs, go ")
    (insert-button
     "back" 'action
     (lambda (_) (org-id-cleanup--do this-step 'previous)))
    (insert "\n")
    (goto-char pt)))


(defun org-id-cleanup--step-save-again ()
  "Step from `org-id--cleanup-do'."
  (insert (format "\n  Deleted %d IDs (out of %d).\n\n\n" org-id-cleanup--num-deleted-ids org-id-cleanup--num-all-ids))
  (insert (format "A log of all changes has been appended to %s\n" org-id-cleanup--log-file-name))
  (insert "\nYou may want to ")
  (insert-button
   "browse" 'action
   (lambda (_) (pop-to-buffer org-id-cleanup--log-buffer)))
  (insert " this file to see, what has been removed from your org-buffers but not saved yet.\n")
  (insert "\n\n\nFinally, if satisfied, you should again save all org buffers, update id locations and save them: ")

  (insert-button
   "go" 'action
   (lambda (_)
     (setq buffer-read-only nil)
     (goto-char (point-max))
     (insert "\n\nSaving buffers ... ")
     (redisplay)
     (org-save-all-org-buffers)
     (insert "done\nUpdating ids ... ")
     (redisplay)
     (org-id-update-id-locations org-id-cleanup--all-files)
     (insert "done\nSaving id locations ...")
     (redisplay)
     (org-id-locations-save)
     
     (insert "done\n\nAssistant done.\n")
     (setq buffer-read-only t))))


;; Some helper functions
(defun org-id-cleanup--insert-files (files)
  "Insert given list of FILES into current buffer using full window width."
  (let ((tab-stop-list '(2 42 82)))
    (dolist (name files)
      (if (> (+ (indent-next-tab-stop (current-column))
                (length name))
             (- (window-width) 10))
          (insert "\n"))
      (tab-to-tab-stop)
      (insert name))))


(defun org-id-cleanup--collect-extra-files (head)
  "Collect and return edited list of extra file.
Argument HEAD is a marker-string that precedes the list of files in buffer."
  (let (file files)
    (goto-char (point-min))
    (search-forward head)
    (delete-trailing-whitespace (point) (point-max))
    (forward-line)
    (while (not (looking-at "---"))
      (setq file (buffer-substring (point) (point-at-eol)))
      (cond
       ((file-directory-p file)
        (setq files (append files (directory-files file t org-agenda-file-regexp))))
       ((file-exists-p file)
        (push file files))
       (t (error "%s is neither a file nor a directory" file)))
      (forward-line))
    files))


(defun org-id-cleanup--collect-ids (head)
  "Collect and return edited list of IDs.
Argument HEAD is a marker-string, that precedes the list of ids in buffer."
  (let ((sample-uuid (org-id-uuid))
        id ids)
    (goto-char (point-min))
    (search-forward head)
    (delete-trailing-whitespace (point) (point-max))
    (forward-line)
    (while (not (= (point) (point-max)))
      (setq id (string-trim (buffer-substring (point-at-bol) (point-at-eol))))
      (when (> (length id) 0)
        (unless (= (length id) (length sample-uuid))
          (error "Id %s does not seem to be a valid uuid" id))
        (push id ids))
      (forward-line))
    ids))


(defun org-id-cleanup--peek-into-id ()
  "Show node with if of current line in other window."
  (interactive)
  (let* ((id (string-trim (buffer-substring (point-at-bol) (point-at-eol))))
         (marker (org-id-find id t)))
    (unless marker
      (error "Cannot find ID %s" id))
    (delete-other-windows)
    (pop-to-buffer (marker-buffer marker) '(display-buffer-below-selected (inhibit-same-window . t)) t)
    (goto-char marker)
    (search-forward id)
    (beginning-of-line)
    (org-show-context 'tree)
    (recenter)
    (other-window 1)
    (message "Context of node with id %s" id)))


(defun org-id-cleanup--step-to-num (step)
  "Return number of current STEP within list of all steps (counting from 0)."
  (- (length org-id-cleanup--all-steps)
     (length (member step org-id-cleanup--all-steps))))


(defun org-id-cleanup--normalize-files (&rest lists)
  "Bring a list of LISTS of filenames in standard form by sorting, removing dups and mapping to true filename."
  (delete-consecutive-dups
   (sort
    (mapcar #'file-truename
            (-flatten
             lists))
    'string<)))


(defun org-id-cleanup--open-log (num-to-be-deleted num-all)
  "Open Log buffer.
NUM-TO-BE-DELETED and NUM-ALL used for explanation."
  (setq org-id-cleanup--log-buffer (find-file-noselect org-id-cleanup--log-file-name))
  (with-current-buffer org-id-cleanup--log-buffer
    (goto-char (point-max))
    (org-mode)
    (insert "\n\n* Log of IDs deleted by org-id-cleanup at ")
    (org-insert-time-stamp nil t t)
    (insert "\n\n")
    (insert (format "  Deleting %d IDs out of %d.\n" num-to-be-deleted num-all))
    (save-buffer)))


(defun org-id-cleanup--append-to-log (id filename point path)
  "Append to Log buffer.
ID, FILENAME, POINT and PATH specify detailed location of the id deleted."
  (with-current-buffer org-id-cleanup--log-buffer
    (insert "\n")
    (insert (format "  - ID :: %s\n" id))
    (insert (format "    - Filename :: %s\n" filename))
    (insert (format "    - Point :: %d\n" point))
    (insert "    - Path to node:\n")
    (dolist (ti path)
      (insert (format "      - %s\n" ti)))))


(defun org-id-cleanup--write-log ()
  "Write Log buffer to its file."
  (with-current-buffer org-id-cleanup--log-buffer
    (save-buffer)))


(provide 'org-id-cleanup)

;; Local Variables:
;; fill-column: 75
;; comment-column: 50
;; End:

;;; org-id-cleanup.el ends here
