;;; Commentary:

;; This spot of code add warnings to the byte compiler to report on possible
;; simplifications in the code compiled.  Eg. (kill-buffer (current-buffer))
;; can be simplified to (kill-buffer nil).
;;
;; The reports include
;;
;;     char-before           \
;;     char-after            | argument `point' omitted
;;     push-mark             /
;;     delete-windows-on     `current-buffer' omitted for Emacs 23 up
;;     eq nil                can be `null'
;;     equal 'sym            can be `eq'
;;     forward-word          \ count==1 can be omitted for Emacs 22 and
;;     backward-word         /   XEmacs 21 up
;;     kill-buffer           `current-buffer' nil or omitted for Emacs 23
;;     lisp-indent-function  `declare' within macro for Emacs 23 up
;;     princ "\n"            `terpri'
;;     re-search-forward     \ constant pattern `search-forward'
;;     re-search-backward    /   or `search-backward'
;;     search-forward        \
;;     search-backward       | `point-min' or `point-max' limit nil
;;     re-search-forward     |
;;     re-search-backward    /
;;     switch-to-buffer      to `(other-buffer)' can be nil
;;     up-list               \ count==1 can be omitted for Emacs 21 up
;;     down-list             /
;;     write-region          point-min to point-max can be nil for Emacs 22 up
;;
;;
;; Things like `delete-windows-on' which are version-dependent are reported
;; only when the Emacs in use allows the simplification.  So
;; `delete-windows-on' is only reported in Emacs 23 up, and the message
;; includes a note that it's for 23 up.
;;
;; The version-dependent bits might have scope for a kind of reverse check,
;; warning about a feature newer than a specified target Emacs supports.
;; Except conditionalized code would trigger it far too often.  There might
;; also be scope to declare a target Emacs version level, so as not to warn
;; about simplifications only possible in newer Emacs.
;;
;; Checks not done:
;;
;; `local-variable-p' argument `current-buffer' can be omitted in Emacs, but
;; not in XEmacs.  Reporting that would only spam out dual-targeted code.

