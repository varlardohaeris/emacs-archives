;;; Commentary:
;;
;; `M-x completion-ignored-build-mode' makes on-the-fly additions to
;; `completion-ignored-extensions' to ignore some built files.  See its
;; docstring for details.
;;
;; The idea is to give quicker access to source files.  For example typing
;; "Mak" Tab can complete to "Makefile.am", ignoring the "Makefile.in"
;; created by Automake, and ignoring a "Makefile" created by configure if
;; building in the source dir.
;;
;; The effect is best when an extension like say .c is sometimes a source
;; file and sometimes a generated file.  A foo.c file could be generated
;; from Perl foo.xs or yacc foo.y or lex foo.l etc.  You don't want to
;; ignore all .c files since a given directory might have a mix of manual
;; and generated C files.  `completion-ignored-build-mode' sets up to ignore
;; just the built ones.
;;
;; Currently there's no way to customize the built-file vs source-file rules
;; applied, but you can modify `completion-ignored-build-apply' without too
;; much trouble.  One reason not to customize yet is that pressing
;; `completion-ignored-extensions' into service for ignoring is not an ideal
;; mechanism.  What's really wanted is to ignore certain individual files,
;; not whole suffixes.  It works well enough in practice, but better ought
;; to be possible.
;;
;; Bugs:
;;
;; When a directory name is completed together with a filename, the dynamic
;; ignores are not applied because the directory name is not yet expanded.
;; Pressing Tab again after the directory has expanded should ignore the
;; built files.

