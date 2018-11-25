This package has some similarities to Emacs' Changelog support,
but it is designed for a different purpose.  It allows users
to keep a collection of simple logs or diaries ("steno pads")
in a single directory, and to add new entries to them quickly.

To install this package, copy steno.el to a directory on your
load-path and optionally compile it (it is not speed-critical),
then add the following to your .emacs file, site-start.el, or any
other relevant place:

(autoload 'steno "steno" nil t)
(autoload 'steno-view "steno" nil t)

You can bind either or both of these to menus or to key strokes.
If you wanted to bind 'steno to "\C-cs", for example, you could
use the command

(global-set-key "\C-cs" 'steno)

You may also want to change the value of the 'steno-pad-directory
variable.  By default it is set to $HOME/.steno, which is fine for
Unix users, but DOS/Windows users might prefer a different default.
The 'steno or 'steno-view functions will offer to create the
directory if it doesn't already exist.

Use "M-x steno-view" to open a steno pad without starting a new
entry (though you can always do so later), or "M-x steno" to start
a new entry right away.  Inside a steno pad, the most important key
strokes are "\C-c\C-a" to add a new time-stamped entry, "\C-c\C-n"
to move to the next entry, "\C-c\C-p" to move to the previous
entry, and "\C-c\C-c" to save the steno pad and remove its window
from the display.  'steno-mode is derived from 'text-mode, so any key
sequences defined in 'text-mode-map will be available in
'steno-mode as well.

I may add menu support and multi-file searching in a future release.
