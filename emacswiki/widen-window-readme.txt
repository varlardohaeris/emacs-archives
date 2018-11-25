This minor mode, widen window mode, provides a function that widen
selected window automatically.
It was tested only on Emacs 22.

In order to use this minor mode, put this file into
a directory included in load-path,
and add following code to your .emacs.
+------------------------+
(require 'widen-window)
(global-widen-window-mode t)
+------------------------+

You can change the window size ratio by customizing `ww-ratio'.
`ww-ratio' must be greater than 0.0 and less than 1.0 .

If you want to disable widen window mode in a certain
major mode(say `foo-mode'), add `foo-mode' to the variable `ww-nonwide-modes'.

If `ww-width' is non-nil, horizontal window widening is done.
You can turn it off by setting `ww-width' nil.
`ww-height' is the same as.

Window widening function `widen-current-window' is called after the
invocation of a function listed in `ww-advised-functions'.
By adding functions to or removing from this variable, you can
control the invocation of window widening.
