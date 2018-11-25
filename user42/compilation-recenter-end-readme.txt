;;; Commentary:

;; This is a spot of code to recentre compilation windows when a compile
;; finishes, so that the end of output is at the bottom of the window, thus
;; maximizing the visible part.  This is good under Emacs' default scrolling
;; policy where you could be left with only half the window used.

;; Emacsen:
;;
;; Emacs 20 and XEmacs 21
;;     In old Emacs with only a single `compilation-finish-function'
;;     variable, that variable is forcibly set by this code.  See notes in
;;     the `compilation-recenter-end-enable' docstring if you want more than
;;     one finish function.

