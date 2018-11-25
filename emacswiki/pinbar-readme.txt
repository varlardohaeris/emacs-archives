This library provides a minor mode to display tabs in the header
line.  It works on GNU Emacs 21 or later version.

M-x `pinbar-mode' toggle the display of the pin bar, globally.

Installation:
		put the following in .emacs file

 (require 'pinbar)
 (global-set-key "\M-0" 'pinbar-add)
 (pinbar-mode t)

	now you can use ALT-0 to pin a buffer to the pin, and ALT-- ALT-0 to unpin it
