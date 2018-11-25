;;; Commentary:

;; This spot of code helps establish a default `compile-command' for
;; M-x compile etc in particular files.
;;
;; Each function in `compile-command-default-functions' can contemplate the
;; buffer filename, directory, contents, etc, and decide a compile-command
;; which applies.
;;
;; The only included functions as yet setup to run Perl, either just on a
;; script, in a working directory for development, or a test script directly
;; or through the usual MakeMaker test harness.
;;
;; The operative part is really just hacking `hack-local-variables' for a
;; place to establish a default.  The list of functions allows wild tests
;; for what to apply when and to construct a command perhaps with absolute
;; directories etc.
;;
;; See mode-compile.el for a bigger system geared more towards language
;; compiles like gcc etc.
;;
;; Bugs:
;;
;; When a file is renamed with `dired-do-rename' a buffer visiting it
;; follows to the new name but the `compile-command' is not recalculated.
;; This is no good since the old name is left in that string.  Maybe the
;; `compile-command-default-functions' should be re-run in that case, though
;; maybe only if the compile-command was calculated by
;; compile-command-default and hasn't been edited manually later.  Or if
;; always overridden then an edited command would still be in the
;; `compile-history' list.

