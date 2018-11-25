;;; Commentary:

;; This spot of code extends `M-x ffap' to recognise Perl POD F<> filename
;; markup like
;;
;;     F</etc/motd>
;;
;; By itself ffap takes the F< as part of the filename and so doesn't find
;; it.  See `ffap-pod-F-enable' below for more.

