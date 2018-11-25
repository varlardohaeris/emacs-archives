F1 is a useless key on my keyboard, because I don't use it in
Emacs, so I thought I bound some kind of help function on it.

I thought some kind of help which is less intrusive than the
default one (doesn't open a new window, frame, etc.) would be
useful, so I made one using tooltips.

When using in a Lisp program the tooltip is displayed without
osbcuring the position of the cursor and it is dismissed
automatically when the user continues typing, so it doesn't disrupt
the current window configuration like the current help does.

How it works:

If the cursor is ON a symbol then help is shown for that symbol.

If the cursor is after or before a symbol then the function symbol
belonging to the containing sexp is used. If no such symbol is
found then a nearby symbol is tried.

If the symbol has both function and variable bindings then both of
them are shown together in the tooltip.

When function help is shown and the cursor is in the argument list
then the relevant argument is highlighted in the documentation.
(Using code copied from Emacs Wiki.)

Support can be added by creating a function with a name like this:

  th-<major-mode>-handler

Currently only Emacs lisp mode is supported.


Tested on Gnu Emacs 21.
