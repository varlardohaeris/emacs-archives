;;; gnusnotes.el --- Adding per-message notes in gnus summary buffer


;; Author: Ruben Berenguel <ruben@mostlymaths.net>
;; Keywords: mail, gnus

;; Version: 0.91

;; Copyright (C) 2012  Ruben Berenguel <ruben@mostlymaths.net>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Still in beta. I've been using it intensively for the past days,
;; with no problems at all. Use it at your own risk. Customize the
;; note file location and to install just

;; (require 'gnusnotes)

;; I have done my best to not use any fanciness. If you'd rather have
;; notes with three fields (like adding the date to the note and
;; highlighting it), it should be pretty straightforward to do so if
;; you know a little emacs lisp. Feel free to do so (in fact, I may
;; add this particular option...)

;; Command suggestion:

;; (require 'key-chord)
;; (key-chord-define-global "nm" 'mostlymaths/gnus-notes)
;; (key-chord-define-global "km" 'mostlymaths/gnus-notes-delete)

;; Only tested in emacs 24.1.1 with NoGnus 0.18. Check at your own
;; risk (it should not harm anything, but as always, beware ;)

;;; Code starts here:

;; This is the main hook to gnus, through the custom-defined field in
;; summary view. To use it, you should add the following to your .gnus
;; file:

;; (setq-default
;;  gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B%s%) %un\n"
;;  )

(defun gnus-user-format-function-n (data)
  (or (mostlymaths/get-gnus-notes (aref data 0)) "")
)

;; The two basic customizations: NOTE text and location of notes file

(setq gnus-summary-notes "NOTE:")

(setq mostlymaths/gnus-notes-file "~/.gnusnotes")

;; Highlighter function. Yes, activating and deactivating hi-lock-mode
;; is needed, don't know why.

(defun mostlymaths/gnus-summary-notes-hl ()
  (interactive)
  (hi-lock-mode -1)
  (hi-lock-mode 1)
  ;(message "Highlighting %s" gnus-summary-notes)
  (highlight-regexp gnus-summary-notes 'warning)
  (highlight-regexp gnus-summary-notes 'italic)
  (highlight-regexp (concat gnus-summary-notes "\.\*") 'default)
)

;; Hooks to reload the highlighter after refresh

(setq gnus-summary-prepare-hook 'mostlymaths/gnus-summary-notes-hl)
(setq gnus-summary-hook 'mostlymaths/gnus-summary-notes-hl)

;; This function loads the notes in a list when a group is loaded

(setq gnus-select-group-hook
      '(lambda ()
	 (setq mostlymaths/gnus-notes-list (mostlymaths/load-gnus-notes))))


(defun mostlymaths/load-gnus-notes ()
  ;; This function loads the notes. Nothing fancy: reads by line and
  ;; breaks by comma. Broken files may not work. Beware.
  (interactive)
  (set-buffer (find-file mostlymaths/gnus-notes-file))
  (goto-char 1)
  (setq moreLines t)
  (setq mostlymaths/gnus-notes-list ())
  (while moreLines
    (beginning-of-line)	       
    (setq p1 (point))
    (if (search-forward "," nil t)
	(progn (setq p2 (- (point) 1))
	       (setq msgid (buffer-substring-no-properties p1 p2))
	       ;(message "%s" msgid)
	       (end-of-line)
	       (setq p1 (point))
	       (setq note (buffer-substring-no-properties (+ 1 p2) p1))
	       ;(message "%s" note)
	       (setq mostlymaths/gnus-notes-list (append mostlymaths/gnus-notes-list (list (cons msgid note))))
	       ;(message "%s" mostlymaths/gnus-notes-list)
	  )
      
      )
    (setq moreLines (= 0 (forward-line 1)))
    )
  (kill-buffer)
  mostlymaths/gnus-notes-list
  )

(defun mostlymaths/get-gnus-notes (msgid)
  ;; This function looks for a msgid and returns the note (or nothing)
					;(message "Get notes: %s" msgid)
  (setq gnus-note "")
  (unless (null mostlymaths/gnus-notes-list)
    (dolist (item mostlymaths/gnus-notes-list)
      ;(message "%s %s %s" item (car item) (cdr item))
      (if (equal (car item) (format "%s" msgid))
	  (progn
	    (setq gnus-note (format "\t NOTE: %s" (cdr item)))
	    (return))
	))
    gnus-note
    ))



(defun mostlymaths/gnus-notes ()
  ;; Function to add or edit a note. All boils down to the function ending in -1
  (interactive)
  (mostlymaths/gnus-notes-1)
  )

;; Function to delete a note. All boils down to the function ending in -1

(defun mostlymaths/gnus-notes-delete ()
  (interactive)
  (mostlymaths/gnus-notes-1 t)
)


(defun mostlymaths/gnus-notes-1 (&optional delete)
  ;; The most meat is here. If there is no note, add one. If there
  ;; already is one, either edit or delete depending on the optional
  ;; argument. All the process is done via the buffer and standard
  ;; movement commands.

  ;(message "%s" (car (gnus-summary-work-articles 1)))
  (setq mostlymaths/gnus-notes-msgid (car (gnus-summary-work-articles 1)))
  (setq mostlymaths/got-message (mostlymaths/get-gnus-notes mostlymaths/gnus-notes-msgid))
  ;(message "Got Message? %s"  mostlymaths/got-message)
  (if (or (equal mostlymaths/got-message nil) (equal mostlymaths/got-message ""))
      ; First clause, note not found. Add new note
      (progn 
	(set-buffer (find-file-noselect mostlymaths/gnus-notes-file))
	(setq gnus-new-note (read-from-minibuffer "Enter Note: "))
	(end-of-buffer)
	(newline-and-indent)
	(insert (concat (format "%s" mostlymaths/gnus-notes-msgid) "," gnus-new-note))
	(goto-char 1)
	(flush-lines "^$")
	(save-buffer)
	(kill-buffer)
	(mostlymaths/load-gnus-notes)
	(gnus-summary-prepare)
	;(gnus-summary-rescan-group)
    )
    ; Else, message found. Defaults to edit
    (if (not delete)
	(progn 
	  (set-buffer (find-file-noselect mostlymaths/gnus-notes-file))
	  (goto-char 1)
	  (search-forward (format "%s" mostlymaths/gnus-notes-msgid nil))
	  (search-forward "," nil)
	  (setq p1 (point))
	  (end-of-line)
	  (setq p2 (point))
	  (setq mostlymaths/old-note (buffer-substring-no-properties p1 p2))
	  (setq mostlymaths/gnus-new-note (read-from-minibuffer "Enter Note: " mostlymaths/old-note))
	  (delete-region p1 p2)
	  (insert (format "%s" mostlymaths/gnus-new-note))
	  (goto-char 1)
	  (flush-lines "^$")
	  (save-buffer)
	  (kill-buffer)
	  (mostlymaths/load-gnus-notes)
	  (gnus-summary-prepare)
	  )
      (progn
	(set-buffer (find-file-noselect mostlymaths/gnus-notes-file))
	(goto-char 1)
	(search-forward (format "%s" mostlymaths/gnus-notes-msgid nil))
	(beginning-of-line)
	(kill-line)
	(goto-char 1)
	(flush-lines "^$")
	(save-buffer)
	(kill-buffer)
	(mostlymaths/load-gnus-notes)
	(gnus-summary-prepare)
	)
      )
    ))

(provide 'gnusnotes)

;;; gnusnotes.el ends here
