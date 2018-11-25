Introduction
------------

This package facilitates when you need to switch from a .cpp file,
for example, to a corresponding .hpp file.  That is, you visit a
.cpp file, type M-x switch-cc-to-h RET, and the corresponding .hpp
file is visited automagically.

It also helps when you have three (or more) file extensions to
switch, for example, you could have .cpp, .hpp and .inc extensions.
So, you could switch from a .cpp file to a .hpp file, from .hpp to
.inc, and from .inc to .cpp again.

For good performance, be sure to byte-compile switch-file.el, e.g.

   M-x byte-compile-file <give the path to switch-file.el when prompted>

This will generate switch-file.elc, which will be loaded instead of
switch-file.el.

switch-file was tested with GNU Emacs 21, 22 and 23.

I don't know if it is compatible with XEmacs.


Usage
-----

To use switch-file, insert in your ~/.emacs:

   (require 'switch-file)
   (setq switch-path (list <PATH-LIST>))

And, for example, to switch from a current .cc file to a .hh
corresponding file (or vice-versa) type:

   M-x switch-cc-to-h RET

As a suggestion for key bindings:

   (global-set-key [f3] 'switch-cc-to-h)

You can also include interactively a new path into `switch-path' option via
`switch-path' command (which see).


Options
-------

Below it's shown a brief description of switch-file options,
please, see the options declaration in the code for a long
documentation.

`switch-path'                Specify a path list for locating files
                             to switch.

`switch-major-mode-alist'    Specify a major mode alist.

To set the above options you may:

a) insert code in your ~/.emacs, like:

	 (setq switch-path '("some-dir/" "other-dir/"))

   This method preserves your default settings when you enter a new
   Emacs session.

b) or use `set-variable' in your Emacs session, like:

	 M-x set-variable RET switch-path RET
	 '("some-dir/" "other-dir/") RET

   This method preserves your settings only during the current
   Emacs session.

c) or use customization, for example:
	 click on menu-bar *Help* option,
	 then click on *Customize*,
	 then click on *Browse Customization Groups*,
	 expand *Convenience* group,
	 and then customize switch-file options.
   This way, you may choose if the settings are kept or not when
   you leave out the current Emacs session.

d) or see the option value:

	 C-h v switch-path RET

   and click the *customize* hypertext button.
   This way, you may choose if the settings are kept or not when
   you leave out the current Emacs session.


Acknowledgments
---------------

Thanks to SeungcheolJung (EmacsWiki) for code correction.

Thanks to Arndt Gulbrandsen (QtMode EmacsWiki) for very first
version.
