;;; Commentary:

;; This is a couple of lines (literally!) to have `variable-at-point'
;; recognise a quoted variable 'foo when point is on the '
;;
;;     (set 'fill-column 70)
;;          ^--- point here
;;
;; This is incorporated in Emacs 24 up and variable-at-point-tick.el is not
;; needed and does nothing there.
;;
;; In Emacs 23.2 for example `variable-at-point' will actually go to the
;; previous symbol, provided it is bound.  For example in the following it
;; gives the preceding `emacs-version',
;;
;;     (list 'emacs-version 'baud-rate)
;;                          ^--- point here

