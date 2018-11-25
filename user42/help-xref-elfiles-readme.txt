;;; Commentary:

;; This spot of code buttonizes "foo.el" and ".emacs" filenames in
;; docstrings.
;;
;; foo.el files available in the `load-path' are buttonized so Ret
;; (`help-follow') visits the file, similar to the buttonized filename of
;; the source file for the function itself.
;;
;; .emacs is buttonized to visit `user-init-file'.  If you're running under
;; "emacs -q" then `user-init-file' is nil and the button throws an error.
;; Is there a way to find the usual init file when running -q ?
;;
;; Filenames in docstrings are uncommon in Emacs itself, but can be found in
;; add-on Lisp packages cross referencing each other.

