Fixes to bugs/anomalies in the `mouse-drag-drag' function (see
commentary to mouse-drag.el) that occur when mouse leaves the
window or frame during a drag.

Is not compatible with XEmacs!!!

Installation:

To get mouse-drag to work with flyspell add the following to your
.emacs

(when (symbolp 'mouse-drag-drag)
  ;; so mouse-yank-at-click works
  (define-key flyspell-mode-map [(mouse-2)] nil)

  ;; so as to not interfere with mouse-drag
  (define-key flyspell-mouse-map [(mouse-2)] nil)
  (define-key flyspell-mouse-map [(down-mouse-2)] 'flyspell-correct-word)
  )

To Do

(1) should really fix drag throw  as well
(2) XEmacs compatibility
