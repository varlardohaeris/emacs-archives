This extension provides a way to use undo steps of
individual file buffers persistently.

Write the following code to your .emacs:

(require 'undo-fu-session)
(global-undo-fu-session-mode)

Now you can record and recover undo-fu-session by typing
C-x C-s (save-buffer) an C-x C-f (find-file).
And then type C-/ (undo).
