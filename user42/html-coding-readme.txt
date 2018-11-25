;;; Commentary:

;; This is a spot of code for Emacs 21 to get the coding system from a HTML
;; <meta> tag when visiting a .html, .shtml or .htm file.  Emacs 22 has this
;; feature already (in sgml-html-meta-auto-coding-function).  html-coding.el
;; notices that, and does nothing there.
;;
;; mm-util.el is used to map a mime charset name in the html to an emacs
;; coding system (mm-util.el is from Gnus, but you don't need to run Gnus).
;; "windows-NNNN" charsets are recognised too, and load the necessary
;; codepage, when available.

