;;; Commentary:

;; This spot of code extends the `M-x man' interactive spec with
;;    * additional completions of man page names and filenames
;;    * default page name at point which recognises more things
;;    * optional transformation some Perl class names
;; See `man-completion-read' docstring for details.
;;
;; Emacs 23.2 has incorporated some of this, but man-completion.el has more
;; features.

