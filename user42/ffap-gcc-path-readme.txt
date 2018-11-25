;;; Commentary:

;; The default `ffap-c-path' is "/usr/include" and "/usr/local/include",
;; but gcc has a separate directory for bits it provides, like float.h.
;; This spot of code extracts gcc's path by parsing the output of "gcc -v".
;;
;; The same could be done for `PC-include-file-path' perhaps, but there's
;; nothing for that yet.

