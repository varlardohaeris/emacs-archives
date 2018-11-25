Introduction
------------

This package is a minor mode to highlight the current line in
buffer.

highline was inspired by:

   linemenu.el		  Bill Brodie <wbrodie@panix.com>
	 Hook function to highlight current line in buffer.

   hl-line.el		  Dave Love <fx@gnu.org>
	 Highlight the current line.

   highlight-current-line.el	  Christoph Conrad <christoph.conrad@gmx.de>
	 Highlight line where the cursor is.

To use highline, insert in your ~/.emacs:

   (require 'highline)

For good performance, be sure to byte-compile highline.el, e.g.

   M-x byte-compile-file <give the path to highline.el when prompted>

This will generate highline.elc, which will be loaded instead of
highline.el.

highline was tested with GNU Emacs 21, 22, 23 and 24, XEmacs 21.4.20, and
Aquamacs Emacs 1.5.


Using highline
--------------

* To customize highline, type:
	 M-x highline-customize RET

* LOCAL highline (see NOTE 1 below):
   + To activate highline locally, type:
	    C-u 1 M-x highline-mode RET

   + To deactivate highline locally, type:
	    C-u 0 M-x highline-mode RET

   + To toggle highline locally, type:
	    M-x highline-mode RET

* GLOBAL highline (see NOTE 1 below):
   + To activate highline globally, type:
	    C-u 1 M-x global-highline-mode RET

   + To deactivate highline globally, type:
	    C-u 0 M-x global-highline-mode RET

   + To toggle highline globally, type:
	    M-x global-highline-mode RET

* INDIRECT highline (see NOTE 2 below):
   + To activate indirect highline, type:
	    C-u 1 M-x highline-view-mode RET

   + To deactivate indirect highline, type:
	    C-u 0 M-x highline-view-mode RET

   + To toggle indirect highline, type:
	    M-x highline-view-mode RET

   + To split window and activate indirect highline, type:
	    M-x highline-split-window-vertically RET
	    M-x highline-split-window-horizontally RET

You can also bind `highline-mode', `global-highline-mode',
`highline-customize', `highline-view-mode',
`highline-split-window-vertically' and
`highline-split-window-horizontally' to some key, like:

   (global-set-key "\C-c-h" 'highline-mode)
   (global-set-key "\C-c-g" 'global-highline-mode)
   (global-set-key "\C-c-c" 'highline-customize)
   (global-set-key "\C-c-v" 'highline-view-mode)
   (global-set-key "\C-c-2" 'highline-split-window-vertically)
   (global-set-key "\C-c-3" 'highline-split-window-horizontally)

NOTE 1: There is no problem if you mix local and global minor mode
	   usage.

NOTE 2: Indirect highline (`highline-view-mode') is useful when you
	   wish to have various "visions" of the same buffer.
	   Indirect highline uses an indirect buffer to get the
	   "vision" of the buffer.  So, if you kill an indirect
	   buffer, the base buffer is not affected; if you kill the
	   base buffer, all indirect buffer related with the base
	   buffer is automagicaly killed.  Also, any text
	   insertion/deletion in any indirect or base buffer is
	   updated in all related buffers.


Example
-------

As an example, try to insert this in your .emacs file:

 (require 'highline)
 (defun highline-mode-on () (highline-mode 1))
 ;; Turn on local highlighting for Dired (C-x d)
 (add-hook 'dired-after-readin-hook #'highline-mode-on)
 ;; Turn on local highlighting for list-buffers (C-x C-b)
 (defadvice list-buffers (after highlight-line activate)
   (save-excursion
     (set-buffer "*Buffer List*")
     (highline-mode-on)))


Hooks
-----

highline has the following hook variables:

`global-highline-mode-hook'
   It is evaluated always when highline is turned on globally.

`highline-mode-hook'
   It is evaluated always when highline is turned on locally.

`highline-view-mode-hook'
   It is evaluated always when indirect highline is turned on.

`highline-load-hook'
   It is evaluated after highline package is loaded.


Options
-------

Below it's shown a brief description of highline options, please,
see the options declaration in the code for a long documentation.

`highline-face'			Specify face used to highlight
					the current line.

`highline-vertical-face'		Specify face used to highlight
					other than current line.

`highline-line'			Specify which part of line
					should be highlighted.

`highline-vertical'			Specify how many vertical
					lines should be highlighted.

`highline-ignore-regexp'		Specify regexp for buffers to
					ignore.

`highline-priority'			Specify highline overlay
					priority.

`highline-view-prefix'		Specify prefix used in the
					indirect buffer name creation.

`highline-keep-highlight'		Non-nil means keep highlight
					on nonselected windows with
					highline mode on.

To set the above options you may:

a) insert the code in your ~/.emacs, like:

	 (setq highline-face 'highlight)

   This way always keep your default settings when you enter a new
   Emacs session.

b) or use `set-variable' in your Emacs session, like:

	 M-x set-variable RET highline-face RET highlight RET

   This way keep your settings only during the current Emacs
   session.

c) or use customization, for example:

   In Emacs 21 or lower:
	 click on menu-bar *Help* option,
	 then click on *Customize*,
	 then click on *Browse Customization Groups*,
	 expand *Editing* group,
	 expand *Highline* group
	 and then customize highline options.

   In Emacs 22 or higher:
	 click on menu-bar *Options* option,
	 then click on *Customize Emacs*,
	 then click on *Browse Customization Groups*,
	 expand *Editing* group,
	 expand *Highline* group
	 and then customize highline options.

   Through this way, you may choose if the settings are kept or not
   when you leave out the current Emacs session.

d) or see the option value:

	 C-h v highline-face RET

   and click the *customize* hypertext button.
   Through this way, you may choose if the settings are kept or not when
   you leave out the current Emacs session.

e) or invoke:

	 M-x highline-customize RET

   and then customize highline options.
   Through this way, you may choose if the settings are kept or not
   when you leave out the current Emacs session.


Acknowledgements
----------------

Thanks to Le Wang <l26wang@gmail.com> for not highlight a line when region
is active.

Thanks to David Reitter <david.reitter@gmail.com> for `highline-face' less
contrastive default values.

Thanks to Stefan Kamphausen <ska@skamphausen.de> and Steven Tate
<state@odnosam.com> for testing.

Thanks to Gwern Branwen <gwern0@gmail.com> for indicating defface
:group attribute.

Thanks to Sandip Chitale <sandip.chitale@brokat.com> for
byte-compilation tests.

Thanks to Stephan Engelke <engelke@gmx.ne> for XEmacs tests.

Thanks to Roman Belenov <roman@nstl.nnov.ru> for `pre-command-hook'
suggestion.

Thanks to Trey Jackson <bigfaceworm@hotmail.com> for
`highline-line' enhancements.

Thanks to Fredrik Sundstroem <fresun-7@sm.luth.se> for
permanent-local overlay property indication.

Thanks to:
   Bill Brodie <wbrodie@panix.com>		   linemenu.el
   Dave Love <fx@gnu.org>			   hl-line.el
   Christoph Conrad <christoph.conrad@gmx.de>   highlight-current-line.el
And to all people who contributed with them.
