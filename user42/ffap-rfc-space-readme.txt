;;; Commentary:

;; M-x ffap doesn't recognise a space in an RFC like "RFC 1234", only say
;; "RFC-1234".  The entry in `ffap-alist' in Emacs 22 and earlier allows it,
;; but the guessing in `ffap-string-at-point' never matched it.  The
;; following spot of code extends both to do that, adding a (thing-at-point
;; 'rfc) as a bonus.
;;
;; Perhaps changing the `file' regexp in ffap-string-at-point-mode-alist
;; would be cleaner.  Or perhaps M-x ffap could tie in with thing-at-point a
;; bit more, and look for things that have "find" handlers.

