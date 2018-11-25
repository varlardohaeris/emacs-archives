;;; Commentary:

;; `M-x align-let' aligns the value parts of bindings in a "let" form or a
;; multi-variable "setq".  It's designed for elisp and scheme but should
;; work with other Lisp variants.  Eg..
;;
;;     (let ((x       1)
;;           (counter 2)
;;           (foo     3))
;;       ...)
;;
;; or
;;
;;     (setq abc   456
;;           xyzzy 789)
;;
;; Forms are recognised and traversed using Emacs usual paren syntax and
;; sexp motion, so it should handle complicated value expressions better
;; than align.el's regexps.
;;
;; `M-x align-let-region' can act on all `let' and `setq' forms in the
;; region from point to mark (or any region if called from Lisp code).  It
;; might help you clean up a whole file of code.

