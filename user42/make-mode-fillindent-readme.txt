;;; Commentary:

;; This spot of code helps makefile-mode in Emacs 21 and 22 have
;; fill-paragraph (M-q) work on indented comments like
;;
;;	foo:
;;		# this is
;;		# some comment
;;		echo hi
;;
;; `makefile-mode' in Emacs 21 and 22 has a special makefile-fill-paragraph
;; for filling.  It recognises "#" comments at the start of a line, but does
;; nothing on indented comments.  Doing nothing is particularly
;; disconcerting if you use filladapt.el, because filladapt-debug shows a
;; correct prefix+paragraph analysis yet M-q has no effect.
;;
;; Whether indented comments is a good idea is another matter.  "make" runs
;; them with the shell, and it ignores them.  The happy side-effect though
;; is to get an echo from make, so you see the comments as the rule
;; proceeds, whereas unindented makefile comments are consumed by make.
;;
;; Paragraph identification in Emacs 21 and XEmacs 21 makefile-mode isn't
;; really setup for indented comments, and no attempt is made to do anything
;; about that here.  The suggestion is to use filladapt which gets it right
;; (or is easier to configure if it doesn't).
;;
;; If you find you don't like what this code does then to undo try
;;
;;     M-x unload-feature
;;     make-mode-fillindent
;;

