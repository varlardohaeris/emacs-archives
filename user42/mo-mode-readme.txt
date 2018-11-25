;;; Commentary:

;; This code adds decoding and viewing of .mo and .gmo message catalogue
;; files from Gettext (http://www.gnu.org/software/gettext/).
;;
;; A format-alist entry does the conversion between binary MO data and PO
;; style text in a buffer using the msgfmt and msgunfmt programs.  The
;; `mo-mode' function ensures the transformation has been applied and
;; switches to either po-mode or text-mode.
;;
;; In Emacs 22 once format-alist is setup it actually works to have
;; `po-mode' directly for .mo files, instead of the helper `mo-mode'.  But
;; `mo-mode' makes it easier to autoload the code here, it ensures decoding
;; works from tar-mode and archive-mode, and it fixes multibyteness when
;; visiting in Emacs 21.
;;
;; Note that `tar-mode' (as of Emacs 22) doesn't follow `buffer-file-format'
;; when saving so if you rewrite a .mo inside a .tar you get the PO text.
;; This afflicts all file format things, including the builtin
;; `enriched-mode'.  So don't do that.  `archive-mode' saving is ok.

