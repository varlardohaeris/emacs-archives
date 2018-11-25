;;; Commentary:

;; This couple of lines adds ".lz4" to jka-compr for automatic decompression
;; in `auto-compression-mode'.
;;
;; .lz4 files are compressed or decompressed with the "lz4" program (which
;; in Debian is in the liblz4-tool package).  If you don't have that program
;; then this code still loads and makes its setup, but visiting a .lz4 file
;; gets an error and shows raw bytes.

