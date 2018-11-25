;;; Commentary:

;; This package is various added or removed patterns for `filladapt-mode'
;; paragraph filling.  The functions are designed to go in a mode hook and
;; operate buffer-local, but can also be used interactively for occasional
;; changes to the patterns, or globally to have everywhere.
;;
;; The setups work whether or not filladapt has loaded yet and do nothing if
;; you haven't turned on filladapt or if you don't have it at all.
;;
;; Most of the patterns added or removed are fairly simple and the
;; descriptions of what's done tend to be longer than the actual code, but
;; the subtlety comes from acting buffer-local and deferring until filladapt
;; is actually used.

