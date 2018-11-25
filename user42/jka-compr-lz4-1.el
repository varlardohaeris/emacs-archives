;;; jka-compr-lz4.el --- .lz4 for jka-compr

;; Copyright 2016 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 1
;; Keywords: data
;; URL: http://user42.tuxfamily.org/jka-compr-lz4/index.html
;; EmacsWiki: AutoCompressionMode

;; jka-compr-lz4.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; jka-compr-lz4.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This couple of lines adds ".lz4" to jka-compr for automatic decompression
;; in `auto-compression-mode'.
;;
;; .lz4 files are compressed or decompressed with the "lz4" program (which
;; in Debian is in the liblz4-tool package).  If you don't have that program
;; then this code still loads and makes its setup, but visiting a .lz4 file
;; gets an error and shows raw bytes.

;;; Emacsen:

;; Designed for Emacs 20 up and XEmacs 21 up.

;;; Install:

;; Put jka-compr-lz4.el in one of your `load-path' directories and add
;; to your .emacs
;;
;;     (require 'jka-compr-lz4)
;;
;; In recent emacs this only adds to the variables of jka-cmpr-hook.el, but
;; in past emacs it loads all of jka-compr.el.  To defer loading jka-compr
;; is enabled (assuming you don't load jka-compr from your .emacs already
;; anyway) then try instead,
;;
;;     (if (featurep 'jka-cmpr-hook)
;;         (require 'jka-compr-lz4)
;;       (eval-after-load "jka-compr" '(require 'jka-compr-lz4)))
;;
;; There's an autoload cookie for this below if you install via
;; `M-x package-install' or know how to use `update-file-autoloads'.

;;; History:
;; 
;; Version 1 - the first version

;;; Code:

;;;###autoload (if (featurep 'jka-cmpr-hook)
;;;###autoload     (require 'jka-compr-lz4)
;;;###autoload   (eval-after-load "jka-compr" '(require 'jka-compr-lz4)))

;; builtin jka-cmpr-hook.el allows additions,
;; in earlier Emacs must load the full jka-compr.el
(unless (featurep 'jka-cmpr-hook)
  (require 'jka-compr))

;; perhaps builtin in future emacs
(unless (member t (mapcar (lambda (vec)
                            (let ((regexp (elt vec 0)))
                              (numberp (string-match regexp "foo.lz4"))))
                          jka-compr-compression-info-list))

  ;; https://github.com/lz4/lz4/wiki/lz4_Frame_format.md
  ;; magic number is 4-bytes 0x184D2204, little endian
  (add-to-list 'jka-compr-compression-info-list
               ["\\.lz4\\'"
                "lz4 compressing"   "lz4" ("-c" "-q")      ;; -c to stdout
                "lz4 uncompressing" "lz4" ("-c" "-q" "-d") ;; -d decompress
                nil           ;; append flag
                t             ;; major mode use filename without .lz4
                "\004\021\115\014"])

  (cond
   ;; recent jka-cmpr-hook.el has `jka-compr-update' for use when changing
   ;; `jka-compr-compression-info-list'
   ((fboundp 'jka-compr-update)
    (jka-compr-update))

   ;; if already enabled then toggle to get our addition recognised
   ;; (note no `auto-compression-mode' variable in xemacs 21)
   (jka-compr-added-to-file-coding-system-alist
    (auto-compression-mode 0)
    (auto-compression-mode 1))))

;;-----------------------------------------------------------------------------

;; LocalWords: lz liblz compr

(provide 'jka-compr-lz4)

;;; jka-compr-lz4.el ends here
