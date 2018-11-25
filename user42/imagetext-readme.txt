;;; Commentary:

;; This code extends `image-mode' and `auto-image-file-mode' to show text
;; comment parts of PNG, JPEG and GIF files.  The image size is shown too
;; for all image types.
;;
;; There's no way to edit the image text, but of course there's no way to
;; edit the image itself either.
;;
;; Caution: The plain image modes don't change the buffer contents, so you
;; can save under a different filename.  But the text added here breaks
;; that.  In `image-mode' if you switch back to raw with the usual C-c C-c
;; then the text extras are removed too and hopefully it should work to save
;; from there.

