The `igrep-next-error' command is just like the `next-error' Emacs
command, but it also highlights the matched text in the source buffer
if the compilation buffer is an *igrep* buffer.  Similarly, the
`igrep-previous-error' and `igrep-first-error' commands are just like
the `previous-error' and `first-error' Emacs commands, plus
highlighting.

You can customize the `igrep-highlight' user option and the
`igrep-highlight' face, and you can make the
`igrep-highlight-overlay' variable buffer local.

You can also safely replace the Emacs commands' key bindings:
(substitute-key-definition 'next-error 'igrep-next-error
			      (current-global-map))
(substitute-key-definition 'previous-error 'igrep-previous-error
			      (current-global-map))
(substitute-key-definition 'first-error 'igrep-first-error
			      (current-global-map))
