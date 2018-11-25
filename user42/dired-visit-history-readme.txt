;;; Commentary:

;; This is a simple few lines to arrange that files visited from `dired'
;; with
;;
;;     Ret   dired-find-file
;;     v     dired-view-file
;;
;; are added to `file-name-history' and so are available as history in
;; `find-file' and other filename reading.
;;
;; Whether you want dired visits included in the find history is a matter of
;; personal preference.  Including them helps keep the history as all files
;; recently visited, whether you typed a name to C-x C-f or followed a dired
;; (or M-x locate) name.

