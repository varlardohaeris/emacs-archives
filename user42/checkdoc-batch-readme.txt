;;; Commentary:

;; `M-x checkdoc-batch' runs checkdoc.el over the current buffer.  It's not
;; interactive and doesn't change anything but instead presents a report of
;; problems found using a compilation-mode buffer.  `M-x next-error' can
;; step through the problems in the usual compilation-mode style.
;;
;; An accompanying script "emacs-checkdoc-batch" can run checkdoc-batch on
;; .el files from the command line.  It can be run under M-x compile if
;; desired.
;;
;; See the `checkdoc-batch' docstring below for more.

