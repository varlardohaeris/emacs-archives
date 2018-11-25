;;; Commentary:

;; This is a minimal double-entry accounting system using a journal of
;; transactions in a text file.  The file format is a bit Spartan, but it's
;; quite effective for keeping track of personal finances.
;;
;; Each line in the file is a transaction debiting one account and crediting
;; another.
;;
;;    30 Apr 10  400.00   Salary 09/10 -> Bank
;;
;; Accumulated results are display by account and a summary of open account
;; balances at the end.
;;
;; "Double-entry" means amounts are only ever moved from one account to
;; another.  So if you record salary as "credit bank, debit taxable income"
;; then you can be confident all amounts deposited in the bank have gone to
;; taxable income (or wherever).  It's possible to put a wrong figure, but
;; an amount can't be dropped on the floor.
;;
;; See the `accjournal-mode' docstring below for more (including other
;; similar Lisp packages).  See the examples directory in the source .tar
;; for some complete input files.

