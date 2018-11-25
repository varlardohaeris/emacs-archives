 This package just copy from `idle-scroll.el' and with slightly modified
 for use `DocView' mode's remap keystroke.
 Thanks Alex Schroeder.

 Automatically scroll down line.
 You can use function `auto-scroll-mode' to auto scroll current buffer.
 And use `auto-scroll-mode' again can stop scroll.

 Use function `auto-scroll-faster' can make buffer scroll faster.
 Use function `auto-scroll-slower' can make buffer scroll slower.


Installation:

Put auto-scroll.el to your load-path.
The load-path is usually ~/elisp/.
It's set in your ~/.emacs like this:
(add-to-list 'load-path (expand-file-name "~/elisp"))

And the following to your ~/.emacs startup file.

(require 'auto-scroll)

No need more.
