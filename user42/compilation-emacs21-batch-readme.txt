;;; Commentary:

;; This spot of code adds a pattern to `compilation-error-regexp-alist' for
;; the old style Emacs 21 and earlier (including XEmacs 21) batch byte
;; compiling error messages like
;;
;;     While compiling foo-mode in file /my/dir/foo.el:
;;       !! assignment to free variable foo-bar-quux
;;
;; There's only a filename in the message, no line number, so it's not
;; terrifically helpful, but at least in a compilation buffer the messages
;; are highlighted and go to the offending file.
;;
;; If you only ever `M-x byte-compile-file' etc interactively from within
;; Emacs then you won't need this.  It's geared to running
;;
;;     emacs -batch -f batch-byte-compile
;;
;; from a makefile or script.
;;
;; And Emacs 22 and up prints the usual GNU "file:line:column:" style, so
;; needs no special help.

