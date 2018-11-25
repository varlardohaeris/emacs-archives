  A minor mode that maintain margins between the cursor and the top and
  bottom of a window. It scrolls the window if the cursor is moved to close
  to either edge.

  If point is moved to a place not visible in the window, Emacs will
  recenter as usual.

Usage:

  Toggle this minor mode:

	  M-x scroll-margs-mode

  or set margin and enable this minor mode:

	  M-x set-scroll-margs

Install:

  Place "scrl-margs.el" in a directory in your load-path and
  add to your ~/.emacs file:

	  (require 'scrl-margs)
  or
	  (autoload 'scroll-margs-mode "scrl-margs" nil t)
	  (autoload 'set-scroll-margs "scrl-margs" nil t)

  To enable it at start up time, also add to your ~/.emacs file:

	  (scroll-margs-mode 1)
  or
	  (set-scroll-margs 3 3)

  If you have defined your own scrolling command, or otherwise don't want
  scrl-margs to interfere with a command, put a property value to that
  commands function name. The property is `scroll-margs' and the value
  is preferably `no-scroll' but can be anything except nil. Example:

	  (put 'my-recenter-cursor-at-top 'scroll-margs 'no-scroll)

Compatibility

  + This module works with GNU Emacs-19.29, 19.31, 20.6, 20.7, 21.1 and 22.1
    and XEmacs-20.1, 20.4 and 21.1 (and probably all versions inbetween).
    It does not work with XEmacs-21.4.

  + The scroll margin capabilitis of Emacs-20 and later (`scroll-margin',
    `scroll-conservatively' and `scroll-step') and this module lives quite
    happily together and complement each other. This module is more smooth
    than Emacs-20's `scroll-margin' in comint modes and during query replace,
    and seems to handle `mouse-set-point' better than Emacs-22.
    Setting `scroll-conservatively' while using this mode makes scrolling
    over really long lines smoother.

  + It seems to work with `truncate-lines' and `truncate-partial-width-windows'.

  + If works with hidden text in e.g. outline-mode.

  + It seems to work with the "scroll-in-place" module. (Version 1.3 of
    that module redefines the functions `scroll-up' and `scroll-down' with
    new semantics. The scroll-in-place mode is temporarly disabled to
    get the standard behaviour of those functions.)

Incompatibility

  - This module does not cooperate well with the "follow-mode".

Shortcomings

  - Scroll margins cannot be set to zero.

  - Performance. The function `scroll-margs-curr-line' is called after almost
    every motion command and do a lot of vertical motions internally.
    Notisable performance degration when window contains large amounts
    of hidden text, e.g. in outline mode.

-------------------------------------------------------------------

History:

0.4 (2008-09-19) David Andersson <l.david.andersson(at)sverige.nu>
  New e-mail address; Update compatibilivy comments.
0.3 (2003-08-14) David Andersson
  XEmacs-21.1. Somewhat boost `scroll-margs-curr-line'.
0.2 (2002-06-19) David Andersson
  Set `scroll-step'. Improve docs and comments.
0.1 (2002-05-18) David Andersson
  Modularised

-------------------------------------------------------------------

todo: File name is cryptic but within recommended length. Find a better name?
todo: Change prefix "scroll-margs" to "scroll-margins" or something?
todo: Respect a commands 'no-scroll' property more than one event after.
todo: Better support for (optionally) making this a buffer local mode.
todo: Allow margins to be float, then meaning fraction of window height.
todo: Be follow-mode compatible.
todo: Declare vars for customization.
todo: Overstep the margin when it helps keeping active region visible.
todo: Maintain margins when window is resized?
todo: Use hook window-scroll-functions? Defadvice recenter?

-------------------------------------------------------------------
