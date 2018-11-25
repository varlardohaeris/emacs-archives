;;; Commentary:
;;
;; `M-x weblint' runs the "weblint" program on the current buffer to check
;; syntax of HTML.
;;
;; `weblint-after-save-setup' can run weblint automatically whenever saving
;; HTML.
;;
;; Both ways display errors in a compilation-mode buffer.  For Emacs 22 and
;; earlier see compilation-weblint.el to add error regexps for this.  (Emacs
;; 23 matches them already.)

