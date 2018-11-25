If you use both transient-mark-mode and picture-mode, you will
probably realize how convenient it would be to be able to highlight
the region between point and mark as a rectangle.  Have you ever
wished you could see where exactly those other two corners fell
before you operated on a rectangle?  If so, then this program is
for you.

For example, you can set the mark in preparation for a rectangle
command with `C-x r C-SPC', watch the highlighted rectangle grow as
you move the cursor to the other corner, and then issue the command
and the rectangle disappears.  Or if point and mark are already set
but you want to see what the region would look like as a rectangle,
try `C-x r C-x' which exchanges point and mark and makes the
highlighted region rectangular.

The default Emacs key-bindings put `point-to-register' on
`C-x r C-SPC' but since that command it is already on `C-x r SPC'
and since it is irresistably intuitive to put `rm-set-mark' on
`C-x r C-SPC', I have taken the liberty of recommending that you
override the default key-bindings.

You can also kill or copy rectangles onto the kill ring which is
convenient for yanking rectangles into ordinary buffers (i.e.  ones
not in picture mode) and for pasting rectangles into other window
system programs (e.g. xterm).  These keys are by default bound to
`C-x r C-w' and `C-x r M-w' by analogy to the normal kill and copy
counterparts.

Finally, there is mouse support for rectangle highlighting by
dragging the mouse while holding down the shift key.  The idea is
that this behaves exactly like normal mouse dragging except that
the region is treated as a rectangle.
