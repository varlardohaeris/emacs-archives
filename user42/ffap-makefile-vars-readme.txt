;;; Commentary:

;; This spot of code lets M-x ffap expand makefile macros $(FOO) in a
;; filename, like
;;
;;     PREFIX = /usr
;;
;;     $(PREFIX)/share/foo
;;
;; With point on the "$(PREFIX)/share/foo" an M-x ffap expands to offer
;; "/usr/share/foo".  This is good for constructed filenames in makefiles.
;;
;; Macros are expanded from the definitions in the file and also from
;; `process-environment' like "make" does.  There's no support for the
;; various smart expansions GNU make can do though.

