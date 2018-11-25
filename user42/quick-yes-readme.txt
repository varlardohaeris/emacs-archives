;;; Commentary:

;; This spot of code binds M-y and M-n to answer "yes" or "no" to a
;; `yes-or-no-p' question.
;;
;; Some `yes-or-no-p' questions can be disabled, but if you leave them
;; enabled as a reminder or if sometimes important then M-y is a good
;; shortcut for accepting.
;;
;; If you start typing "y" or "ye", etc, you can still use M-y to finish it.
;; Typing "y" is easy to do if you don't immediately notice it's
;; `yes-or-no-p' instead of `y-or-n-p'.
;;
;; quick-yes.el only affects a `yes-or-no-p' question in the minibuffer, it
;; doesn't change a window system dialog box (as used by `yes-or-no-p' when
;; the invoking action was a mouse button press instead of something from
;; the keyboard).

