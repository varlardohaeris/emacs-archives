`shell-command+' is a `shell-command' substitute, that extends the
regular Emacs command with several features.  After installed,
configure the package as follows:

	(global-set-key (kbd "M-!") #'shell-command+)

A few examples of what `shell-command+' can do:


	> wc -l

Count all lines in a buffer, and display the result in the
minibuffer.


	.. < ls -l

Replace the current region (or buffer in no region is selected)
with a directory listing of the parent directory.


	| tr -d a-z

Delete all instances of the charachters a, b, c, ..., z, in the
selected region (or buffer, if no region was selected).


	... make

Run Eshell's make (via `compile') in the parent's parent
directory.

See `shell-command+'s docstring for more details on how it's input
is interpreted..