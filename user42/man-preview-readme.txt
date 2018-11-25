;;; Commentary:

;; M-x man-preview displays a formatted preview of man nroff source using
;; "man -l".  The best feature is that when re-previewing the same file or
;; buffer the existing position in the preview is preserved, so if you've
;; changed the source only a little you should still be quite close to where
;; you were in the preview, to see how the change has come out.
;;
;; M-x man with "-l filename" does almost the same as this, but depends on
;; having a disk copy of a buffer, so can't work out of tar-mode members
;; etc.

