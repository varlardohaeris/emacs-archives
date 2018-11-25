This client runs "mogo" in a buffer *mogo* and sets the buffer's
major mode to mogo-mode.  MoGo mode redefines RET such that the
player can directly use it on a board to submit the corresponding
move.  If RET is pressed on the last line, then the line is sent to
mogo - after a number of convenience replacements have been made.

Currently, mogo's output is not filtered: it simply appears in the
buffer as mogo sends it.


Installation:

Put (load-library "PATH_TO_MOGO/mogo") in your .emacs file.  This
loads mogo.elc if a byte compiled version is found or mogo.el,
otherwise.
Alternatively, put mogo.el(c) in a directory on the load-path and
say (require 'mogo).
