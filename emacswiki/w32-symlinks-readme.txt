This file is intended to be used with NTEmacs 21, i.e. GNU Emacs 21
compiled as a native Microsoft Windows application and running on
Windows.  It should cause no harm on other platforms and might be
useful when accessing a Windows file system from another OS, but I
have not tested such use.  It provides support for symbolic links
on Microsoft Windows platforms by allowing Emacs to handle Windows
shortcut files transparently as symbolic links in the same way that
Windows itself does, by making .lnk files names "magic".

It contains functions to parse Windows .lnk "shortcut" (and also
obsolete Cygwin-style "symlink") files, entirely in Lisp.  It allows
`dired' to follow symbolic links when using either ls-lisp (the
default) or an external Cygwin ls program.  When run on Windows, it
also implements the missing `dired-do-symlink' command to make
symbolic links.

INSTALLATION ======================================================

Put this file (w32-symlinks.el) somewhere in your load-path and
byte-compile it.  Then choose one of the following options to load
w32-symlinks.  Note that, by default, w32-symlinks supports dired
only; see option 3 below.

1. To provide symlink support for dired only, using the STANDARD
   preloaded version of the NTEmacs 21 or later ls-lisp library,
   put this in your .emacs:

   (add-hook 'dired-load-hook
	        (lambda () (require 'w32-symlinks)))

2. To provide symlink support for dired only, using a version of
   GNU Emacs other than NTEmacs, or using the latest version of the
   ls-lisp library from my web site (which must first be installed
   as per its instructions), put this in your .emacs:

   (add-hook 'dired-load-hook
  	        (lambda ()
	         (load "ls-lisp")
	         (require 'w32-symlinks)))

3. To provide symlink support for GNU Emacs 21 in general
   (including dired), put this in your .emacs:

   (require 'w32-symlinks)

   Also execute both the above sexp (by putting point at the end of
   the sexp and pressing C-x C-e, which runs the command
   eval-last-sexp) and the following sexp

   (customize-option 'w32-symlinks-handle-shortcuts)

   Turn the option on and save the setting for future sessions.
