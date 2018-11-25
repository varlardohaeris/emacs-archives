`mark-lines' provides whole line selection functionality.  The behaviour of
triple-clicking mouse-1 is used as a model.

In (GNU |X)Emacs, when you triple click on a line, that line is
automatically selected, and you go into a mode of region selection where
lines are added to the region when you move the point up or down the buffer
by dragging the mouse.  The original line always remains selected, and new
lines are added to the region based on the vertical position of point
relative to the original line.

This package seeks to duplicate that behaviour with the keyboard, with some
improvements.  See usage for details.

Installation:

1. Add this file to a directory in your `load-path'

2. Add (require 'mark-lines) to your ~/.emacs

3. Add your desired key-bindings to your ~/.emacs, e.g.:

    (global-set-key [(control x) (control p)] 'mark-lines-previous-line)
    (global-set-key [(control x) (control n)] 'mark-lines-next-line)
