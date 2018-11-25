This package provides Midnight Commander style emulation for Emacs.  Midnight
Commander is a UNIX application with a User Interface similar to the old DOS
program named Norton Commander.  Basically this provides the user with a
quick, two pane interface to their file system.  I wanted to model it after
Midnight Commander because it is Free Software (Norton Commander is not) and
recent UNIX/Linux users will generally be more familiar with Midnight
Commander.  Users of dired will be familiar with mc because dired features
are still present.

There is Midnight Commander style package which does not use dired.  It is
named nc.el and is located at:
ftp://ftp.math.ohio-state.edu/pub/users/ilya/emacs.  It generally tries to
target the DOS equivalent Norton Commander.

Install/Usage

Add the lines (require 'mc) to your .emacs file.

In order to run mc just type M-x mc.  This will invoke mc.

The standard Midnight Commander key bindings should now be available.

TAB -> Change between windows

F2      -> Change directory
F3/F4   -> Open/Edit
F5      -> Copy
F6      -> Move/Rename
F8      -> Delete
F10     -> Quit
