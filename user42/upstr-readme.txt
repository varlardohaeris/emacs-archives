;;; Commentary:
;;
;; This is a variation of C-M-u `backward-up-list' which will go up out of a
;; string or TeX maths $..$ as well as out of a nested list.
;;
;; This is helpful if you want to move up but point happens to be within a
;; string.  The plain `backward-up-list' doesn't recognise strings and will
;; often error out instead of moving.  `backward-up-list' also doesn't
;; recognise TeX maths $..$, but `forward-sexp' etc do and it's convenient
;; to have them consistent.
;;
;; See the `upstr-up-list-or-string' and `upstr-backward-up-list-or-string'
;; docstrings for details.

