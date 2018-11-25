;;; Commentary:

;; This spot of code runs the sqlite or sqlite3 program to ".dump" a
;; database file as SQL text for viewing and editing.  Put `sqlite-dump' in
;; `auto-mode-alist' to automatically visit desired files this way.
;;
;; The `buffer-file-format' mechanism is used so the SQL is re-run to save
;; changes.  See the `sqlite-dump' docstring for details.

