This package provides the ability of editing part of the current
buffer in a new buffer whose mode is different from the current
buffer's mode.  The new buffer is called a "subedit buffer", and
the current buffer is called the subedit buffer's "original
buffer".  The "mode" here is either major mode or minor mode.

This ability is different from the ability of having many major
modes in a buffer or many indirect buffers at the same time.
Subedit provides also the ability of stripping line prefixes and
suffixes, which the latter can't provide.

Todo:

   - Add warnings when a buffer having subedit buffers that are
     changed is being killed

   - If a subedit buffer itself is changed and is being killed,
     warn about that
