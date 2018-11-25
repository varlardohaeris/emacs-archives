When working with several frames, I was always annoyed that
switch-to-buffer commands from frame A interfered with the ordering
of the buffers when doing C-x C-b in frame B.  To see this, start
Emacs, then create a buffer (C-x b aaa RET).  Then, create a new
frame and create two new buffers from the second frame (C-x b bbb
RET, then C-x b ccc RET).  Now, go back to the first frame and do
C-x C-b.  I wanted Emacs to show the *scratch* buffer and aaa as
the top two buffers, but the buffers bbb and ccc get in between.

This little package solves this problem.

This package doesn't change the behavior of the other-buffer
function.  This means that the default buffer offered when you do
C-x b (switch-to-buffer) doesn't change.  This behavior was `wrong'
with Emacs 19 and has been `corrected' for Emacs 20.  It is not
possible to fix this for Emacs 19 without hacking the C code.

TODO:

- Deal with bury-buffer correctly.  (This means to put the buffer
  at the bottom of the list, right?)
