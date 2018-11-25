By default, a window within an Emacs frame must be selected by
typing `C-x o' (other-window) or by clicking [mouse-1] on the mode
line or the buffer itself (mouse-set-point); this corresponds to a
"click to type" window manager policy.  follow-mouse.el implements a
"focus follows mouse" window manager policy, so that a window is
selected when the mouse moves over it.

To enable follow-mouse, put this in your ~/.emacs file:
	(turn-on-follow-mouse)

follow-mouse can be enabled or disabled interactively with the
`M-x turn-on-follow-mouse', `M-x turn-off-follow-mouse', and
`M-x toggle-follow-mouse' commands.

By default, follow-mouse will deselect an active minibuffer window;
to prevent that, just unset the
`follow-mouse-deselect-active-minibuffer' option.

By default, follow-mouse also raises the frame whose window is
selected; to disable that, just unset the
`follow-mouse-auto-raise-frame' option.
