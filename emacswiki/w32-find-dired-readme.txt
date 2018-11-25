This is a light variant of `find-dired' for those who run Emacs on
w32.

Instead of using find, cmd.exe's `dir' command is used. The output
is then transformed to something dired can use, using
`insert-directory' from ls-lisp.el.

It does not support all things that `find-dired' can do, mostly
because the dir-command command used is not as powerful. It is also
"hard coded" into doing recursive directory listing.

Most of the code is ripped from find-dired.el

History

* Version 0.1.1, 2006-01-12, Mathias Dahl

 - Changed the way `dir' is called.
