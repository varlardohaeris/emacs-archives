;;; Commentary:

;; This code extends `makeinfo-buffer' (C-c C-m C-b) in two ways,
;;
;;     1. Display its result with `Info-mode', rather than a raw buffer.
;;
;;     2. Follow a TeX-master variable for the top-level .texi of a
;;        multi-file document.
;;
;; In Emacs 22 item 1 has been adopted, so just TeX-master is added there.
;; See the docstring of `makeinfo-info-makeinfo-buffer' below for details.
;;
;; `makeinfo-region' (C-c C-m C-r) is unchanged, it still shows the raw
;; foo.info in fundamental mode.  This is probably good enough, since
;; usually only small sections of a document can be processed in isolation
;; anyway.

