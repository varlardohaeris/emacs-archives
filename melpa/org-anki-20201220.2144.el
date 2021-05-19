;;; org-anki.el --- Synchronize org-mode entries to Anki -*- lexical-binding: t -*-
;;
;; Copyright (C) 2020 Markus Läll
;;
;; URL: https://github.com/eyeinsky/org-anki
;; Package-Version: 20201220.2144
;; Package-Commit: efa9e3aa2d768c00440f745192aba6672b28d086
;; Version: 0.0.2
;; Author: Markus Läll <markus.l2ll@gmail.com>
;; Keywords: outlines, flashcards, memory
;; Package-Requires: ((emacs "24.4") (request "0.3.2") (ox-slimhtml "0.4.1"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Minor mode to synchronize org-mode entries to Anki via AnkiConnect.
;;
;; See https://github.com/eyeinsky/org-anki for more.


;;; Code:

(require 'json)
(require 'org)
(require 'request)
(require 'org-element)
(require 'ox-slimhtml)

;; Constants

(defconst org-anki-prop-note-id "ANKI_NOTE_ID")
(defconst org-anki-prop-deck "ANKI_DECK")


;; Stolen code

;; Get list of global properties
;;
;; From:
;;   https://emacs.stackexchange.com/questions/21713/how-to-get-property-values-from-org-file-headers
(defun org-anki--global-props (&optional name buffer)
  "Get the plists of global org properties by NAME in BUFFER.

Default NAME is \"PROPERTY\", default BUFFER the current buffer."
  (unless name (setq name "PROPERTY"))
  (with-current-buffer (or buffer (current-buffer))
    (org-element-map (org-element-parse-buffer) 'keyword
      (lambda (el) (when (string-match name (org-element-property :key el)) el))
      nil t)))

(defun org-anki--get-global-prop (name)
  "Get global property by NAME."
  (plist-get (car (cdr (org-anki--global-props name))) :value))


;; Talk to AnkiConnect API:

(defun org-anki-connect-request (body callback)
  "Perform HTTP GET request to AnkiConnect's default http://localhost:8765.

BODY is the alist json payload, CALLBACK the function to call with result."
  (request
    "http://localhost:8765" ; This is where AnkiConnect add-on listens.
    :type "GET"
    :data (json-encode body)
    :sync t
    :headers '(("Content-Type" . "application/json"))
    :parser 'json-read

    :error
    (cl-function
     (lambda (&rest _args)
       (debug "Error response in variable '_args'")))

    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (funcall callback data)))))

(defun org-anki--body (action params)
  "Wrap ACTION and PARAMS to a json payload AnkiConnect expects."
  `(("version" . 6)
    ("action" . ,action)
    ("params" . ,params)))

(defun org-anki--create-note (front back deck)
  "Create an `addNote' json structure to be added to DECK with card FRONT and BACK strings."
  (org-anki--body
   "addNote"
   `(("note" .
      (("deckName" . ,deck)
       ("modelName" . "Basic")
       ("fields" .
        (("Front" . ,front)
         ("Back" . ,back)))
       ("options" .
        (("allowDuplicate" . :json-false)
         ("duplicateScope" . "deck"))))))))

(defun org-anki--update-note (id new-front new-back)
  "Create an `updateNoteFields' json structure with integer ID, and NEW-FRONT and NEW-BACK strings."
  (org-anki--body
   "updateNoteFields"
   `(("note" .
      (("id" . ,id)
       ("deckName" . "org-mode")
       ("fields" . (("Front" . ,new-front) ("Back" . ,new-back))))))))

(defun org-anki--delete-notes (ids)
  "Create an `deleteNotes' json structure with integer IDS list."
  (org-anki--body "deleteNotes" `(("notes" . ,ids))))


;; Get card content from org-mode:

(defun org-anki--entry-content-until-any-heading ()
  "Get entry content until any next heading."
  ;; We move around with regexes, so restore original position
  (save-excursion
    ;; Jump to beginning of entry
    (goto-char (org-entry-beginning-position)) ;; was: (re-search-backward "^\\*+ .*\n")
    ;; Skip heading
    (re-search-forward ".*\n")
    ;; Possibly skip property block until end of entry
    (re-search-forward ":properties:\\(.*\n\\)*:end:" (org-entry-end-position) t)
    ;; Get entry content
    (buffer-substring-no-properties (point) (re-search-forward "\\([^*].*\n\\)*" nil t))))

(defun org-anki--string-to-html (string)
  "Convert STRING (org element heading or content) to html."
  (org-export-string-as string 'slimhtml t nil))

(defun org-anki--report-error (format error)
  "FORMAT the ERROR and prefix it with `org-anki error'."
  (let ((fmt0 (concat "org-anki error: " format)))
    (message fmt0 error)))

;; Public API, i.e commands what the org-anki user should use:

;;;###autoload
(defun org-anki-sync-entry ()
  "Synchronize single entry.
Tries to add, or update if id property exists, the note."

  (interactive)
  (let* ((front    (org-anki--string-to-html (org-entry-get nil "ITEM")))
         (maybe-id (org-entry-get nil org-anki-prop-note-id))
         (deck     (org-anki--get-global-prop org-anki-prop-deck))
         (back     (org-anki--string-to-html (org-anki--entry-content-until-any-heading))))

    (cond
     ;; id property exists, update
     (maybe-id
      (org-anki-connect-request
       (org-anki--update-note maybe-id front back)
       (lambda (arg)
         (let ((the-error (assoc-default 'error arg)))
           (if the-error
               (org-anki--report-error
                "Couldn't update note, received: %s"
                the-error)
             (message "org-anki says: note succesfully updated!"))))))
     ;; id property doesn't exist, try to create new
     (t
      (org-anki-connect-request
       (org-anki--create-note front back deck)
       (lambda (arg)
         (let ((the-error (assoc-default 'error arg))
               (the-result (assoc-default 'result arg)))
           (cond
            (the-error
             (org-anki--report-error
              "Couldn't add note, received error: %s"
              the-error))
            (the-result
             (org-set-property org-anki-prop-note-id (number-to-string the-result))
             (message "org-anki says: note succesfully added!"))
            (t
             (org-anki--report-error "%s"
              "Empty response, it should return new note's id."))))))))))

;;;###autoload
(defun org-anki-delete-entry ()
  "Delete org entry under cursor (the id property must exist).

Will lose scheduling data so be careful"
  (interactive)
  (let*
      ((note-id (string-to-number (org-entry-get nil org-anki-prop-note-id))))
    (org-anki-connect-request
     (org-anki--delete-notes `(,note-id))
     (lambda (arg)
       (let ((the-error (assoc-default 'error arg)))
         (cond
          (the-error
           (error "Couldn't delete note, received error: %s" the-error))
          (t
           (org-delete-property org-anki-prop-note-id)
           (message "org-anki says: note successfully deleted!"))))))))


;; Helpers for development, don't use

(defun org-anki--sync-entry-debug ()
  "Debug command which reloads package before running."
  (interactive)
  (message "org-anki-sync-entry-debug")
  (eval-buffer "org-anki.el")
  (org-anki-sync-entry))

(defun org-anki--delete-entry-debug ()
  "Debug command which reloads package before running."
  (interactive)
  (message "org-anki-delete-entry-debug")
  (eval-buffer "org-anki.el")
  (org-anki-delete-entry))

(defun org-anki--debug-bind ()
  "Define keys for testing."
  (define-key org-mode-map (kbd "C-c C-c") 'org-anki--sync-entry-debug)
  (define-key org-mode-map (kbd "C-d C-d") 'org-anki--delete-entry-debug))

(provide 'org-anki)
;;; org-anki.el ends here
