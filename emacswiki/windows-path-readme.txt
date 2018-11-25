This package lets you use windows-style filenames like "c:/path/file" or
"c:\path\file" in cygwin emacs.  There is a cygwin-mount.el let you use
cygwin style path in native Windows Emacs. This package is a opposite of it.

Installation:

Put in your .emacs or site-start.el file the following lines:
  (require 'windows-path)
  (windows-path-activate)

Compatibility

The package is only tested with Cygwin Emacs 23.1.

How it works:
basically push some functions onto file-name-handler-alist.
They detect filenames expressed in Windows style, and translate
those names into cygwin style.
