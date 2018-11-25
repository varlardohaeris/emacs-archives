This library auto-complete+ extend library auto-complete with let users to
define regexp to ignore files when they expand use
ac-source-files-in-current-dir and ac-source-filename, and filter invalid
symbol when they expand use ac-source-symbols, because there so many garbage
in obarray.

Installation:

Copy auto-complete+.el to your load-path and add to your .emacs:

(require 'auto-complete+)
