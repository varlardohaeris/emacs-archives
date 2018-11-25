The purpose of beginner-mode is to make Emacs less imposing, so people will
try it instead of gedit, kate, et cetera.  One way to do it is to
make it look nicer, but the way I have chosen is to reduce the
amount of UI onscreen and make some other changes.

Beginner-mode makes Emacs look simpler and
cleaner.  It also adds functionality to Emacs that many users
appreciate having in a text editor.  (It is added by enabling minor
modes.  If you don't want them, just comment out the appropriate
lines of this code.)

This is only alpha-quality code, and I'm not confident it's useful
yet.  If you are a beginner to Emacs and use Windows, instead try
the EmacsW32 version of Emacs.

I encourage you to edit this file and to submit your changes back
to the webpage you got it from.  Or send me a patch.

Also, I love feedback, and I would be grateful to hear what you
think of beginner-mode.  Even if you hate it, I would like to hear from you.

Installation:

Save this file to somewhere on your load-path then add (require
'beginner) to your .emacs file.
