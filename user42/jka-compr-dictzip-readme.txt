;;; Commentary:

;; This couple of lines adds .dz "dictzip" to jka-compr for automatic
;; decompression in Emacs 21 and XEmacs 21.  It's not required in Emacs 22
;; up where .dz is already built-in.
;;
;; .dz files are gzip format with compression restarts or something at
;; certain points to help random access.  The plain "gzip" program can be
;; used to read the whole file, as done here.
;;
;; There's no support for compressing to re-write a .dz file.  The "dictzip"
;; program won't write to an stdout pipe as it needs to seek back to fill in
;; a table in the header or something.  Perhaps a wrapper script could go
;; through a temporary file if re-writing was really wanted.
;;
;;
;; ".dict" files are often compressed as ".dict.dz" and are UTF-8 coding.
;; That coding can be setup with
;;
;;     (add-to-list 'file-coding-system-alist '("\\.dict\\'" . utf-8))
;;
;; `modify-coding-system-alist' works too, but will error out in xemacs21
;; where there's no utf-8 built-in, whereas an unknown charset entry is
;; quietly ignored.

