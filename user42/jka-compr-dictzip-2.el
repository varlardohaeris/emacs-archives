;;; jka-compr-dictzip.el --- dictzip .dz for jka-compr

;; Copyright 2011, 2015 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 2
;; Keywords: data
;; URL: http://user42.tuxfamily.org/jka-compr-dictzip/index.html
;; EmacsWiki: AutoCompressionMode

;; jka-compr-dictzip.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; jka-compr-dictzip.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.

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

;;; Emacsen:

;; Designed for Emacs 21 and XEmacs21, works in Emacs 20.
;; Not required and does nothing in Emacs 22 and up.

;;; Install:

;; Put jka-compr-dictzip.el in one of your `load-path' directories and add
;; to your .emacs
;;
;;     (require 'jka-compr-dictzip)
;;
;; To defer loading the code until jka-compr is enabled (assuming you don't
;; do that from your .emacs already) then try instead
;;
;;     (eval-after-load "jka-compr" '(require 'jka-compr-dictzip))
;;
;; There's an autoload cookie for this below if you install via
;; `M-x package-install' or know how to use `update-file-autoloads'.

;;; History:
;; 
;; Version 1 - the first version
;; Version 2 - new email

;;; Code:

;;;###autoload (eval-after-load "jka-compr" '(require 'jka-compr-dictzip))

(require 'jka-compr)

;; already in emacs 22
(unless (member t (mapcar (lambda (vec)
                            (let ((regexp (elt vec 0)))
                              (numberp (string-match regexp "foo.dz"))))
                          jka-compr-compression-info-list))

  ;; The "dictzip" program can't compress to stdout, needs to seek,
  ;; otherwise could have: "compressing" "dictzip" ("-c")

  (add-to-list 'jka-compr-compression-info-list
               ;; this element as per emacs 22
               ["\\.dz\\'"
                nil nil nil
                "uncompressing" "gzip" ("-c" "-q" "-d")
                nil t "\037\213"])

  ;; if already enabled then toggle to get our addition recognised
  ;; (note no `auto-compression-mode' variable in xemacs 21)
  (when jka-compr-added-to-file-coding-system-alist
    (auto-compression-mode 0)
    (auto-compression-mode 1)))

;;-----------------------------------------------------------------------------

;; LocalWords: dictzip dz gzip stdout compr UTF utf charset

(provide 'jka-compr-dictzip)

;;; jka-compr-dictzip.el ends here
