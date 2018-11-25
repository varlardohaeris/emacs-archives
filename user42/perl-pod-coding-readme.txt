;;; Commentary:

;; This code gets an Emacs coding system from the "=encoding" line in Perl
;; POD documentation, either inline in Perl code or a separate pod file.
;;
;; POD files can use utf-8 or utf-16 byte order marker (BOM) sequences and
;; Emacs will recognise those by itself.  But a coding system only by
;; =encoding will generally need some help.

