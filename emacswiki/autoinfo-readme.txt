This minor-mode monitors if something is selected in the buffer
where it is turned on and if the user is idle it fetches
information about the selection in the background and shows it in a
tooltip.

I added an example function which looks up using google the
definition of the word you select. It's not polished, since it's
only a quickly implemented example.


Usage:

Turn on autoinfo-mode, select something either with the keyboard or
with the mouse and wait for the results.

Tested on Emacs 22.
