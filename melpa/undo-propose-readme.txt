undo-propose.el is a package for navigating through your undo history
in a temporary buffer.

To use undo-propose, call "M-x undo-propose" in the buffer you are editing.
This will send you to a new temporary buffer, which is read-only except
for allowing `undo' commands.  Cycle through the list of undo's as normal.
When you are finished, type "C-c C-c" to add the chain of undo's as a
single edit to the undo history.  To cancel, type "C-c C-k".  You can also
ediff the proposed chain of undo's by typing "C-c C-d".
