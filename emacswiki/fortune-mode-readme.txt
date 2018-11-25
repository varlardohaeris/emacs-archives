This file defines a major mode and associated functions for editing
fortune files, i.e. files containing quotations separated by `%'
characters.  It can be used in one of two ways.  The first is as a
major mode when editing such files directly.  Since such files do
not have a consistent extension, you must use file local variables
or `auto-mode-alist' to load it automatically, for example:

(add-to-list 'auto-mode-alist
             '("\\`/usr/share/games/fortunes/" . fortune-mode))

The second way to use it is via the `fortune' command, which opens
a buffer in which you can enter one or more fortunes and then issue
a command which will append them to the fortune file of your
choice.

This simple hack allows you to write fortunes into bases.  This is
very simple, and the only goal was my amusement in emacs-lisp
programming, and having fun with friends citations.

Revision 1.2  1999/07/14 20:49:52  drieu
- Fix minors bugs
- Document more
- Add $HOME in fortune-directory (much cleaner)

Revision 1.3  2000/01/09 Michael Shulman
- Merged with fortune.el by MAS
- Added fortune-newline
- Miscellaneous fixes, improvements
