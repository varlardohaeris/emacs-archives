This code was inspired by tooltip-help.el by Tamas Patrovics.
Use `showtip' to replace the toolitp display code of
`tooltip-help.el'.

Show help information as showtip.
When your text cursor in at a valid symbol, use function
`sh-show-help' can get help information.

`sh-show-help' will get different help information along with
the type of current symbol.
Now can support function, variable, face for emacs-lisp-mode.


Installation:

Put show-help.el to your load-path.
The load-path is usually ~/elisp/.
It's set in your ~/.emacs like this:
(add-to-list 'load-path (expand-file-name "~/elisp"))

And the following to your ~/.emacs startup file.

(require 'show-help)

No need more.
