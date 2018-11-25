Emacs provides 2 commands to edit text in a new buffer: switch-to-buffer
and find-file.  The choice between those commands depends on whether
the text will ultimately be saved to a file, but the choice is made
before the text has been edited.  Each command prompts the user, for
a buffer name or a file name, which must be truly new.  And text
edited via switch-to-buffer is lost without prompting if emacs or the
buffer is killed.

The switch-to-new-buffer command is like switch-to-buffer, but it
generates a new buffer name from a customizable default.  It sets
buffer-offer-save to protect against inadvertant data loss via
kill-emacs.  And it adds a buffer-local query function to protect
against inadvertant data loss via kill-buffer.

switch-to-new-scratch-buffer and switch-to-new-untitled buffer are
convenience commands, for creating new *scratch* buffers (like Emacs)
and new "Untitled" buffers (like other text editors).

switch-to-new-buffer is added to the menu bar Buffers menu.
