;;; Commentary:

;; This spot of code adds a `compilation-error-regexp-alist' pattern to
;; recognise error messages from the "ledger" program,
;;
;;     http://newartisans.com/software/ledger.html
;;
;; such as
;;
;;     Error: "foo.ledger", line 1656: Invalid date string: foo
;;
;; This is only for running ledger from M-x compile.  (ledger.el gives
;; reports with its own `ledger-report-mode' which has more features and
;; isn't based on `compilation-mode'.)

