;;; Commentary:
;;
;; This spot of code avoids line breaks in texinfo source files at places
;; past versions of makeinfo didn't handle quite right.  Current makeinfo is
;; ok so this code is not needed unless you might use a past makeinfo.
;;
;; `texinfo-nobreak-enable' avoids line breaks by adding `texinfo-nobreak-p'
;; to `fill-nobreak-predicate', buffer-local.

