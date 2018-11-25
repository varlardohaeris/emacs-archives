;;; Commentary:

;; This spot of code checks ;;;###autoload cookies in Lisp sources on
;;
;;     - an interactive command as an entrypoint
;;     - defgroup forms
;;     - `put risky-local-variable' and similar
;;
;; See the docstrings below of `checkdoc-autoload-entrypoint',
;; `checkdoc-autoload-defgroup' and `checkdoc-autoload-puts' for details.

