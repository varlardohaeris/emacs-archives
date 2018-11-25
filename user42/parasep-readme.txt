;;; Commentary:

;; This is a few functions to add further paragraph separators to the
;; `paragraph-separate' and `paragraph-start' variables.
;;
;;     parasep-dashes          line of --- dashes separator
;;     parasep-empty-comments  empty comments separator
;;     parasep-perl-pod        pod =foo command separators
;;     parasep-perl-pod-X      X<> index directive separator
;;     parasep-tex-index       \index{} on a line alone
;;     parasep-texinfo-@*      @* line break separator
;;
;; The functions are designed to be used either from a mode hook, or
;; M-x interactively for an occasional change.

