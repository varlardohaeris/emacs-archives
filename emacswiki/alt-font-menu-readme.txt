This file is not part of gnu emacs (yet).

Automatically generate a menu of available fonts constrained by
personal preferences, and without increasing frame size. This menu
is perpended to the options menu but may also be accessed as a
pop-up bound to shift-mouse-1 (see below). Requires at least gnu
emacs 21.1 and probably wont work on xemacs:-(

Installation:

Place this file somewhere in your load path and the following
somewhere in your .emacs file

  (autoload 'alt-mouse-set-font "alt-font-menu"
      "interactively choose font using mouse" t)
  (global-set-key [(shift down-mouse-1)] 'alt-mouse-set-font)

and set the parameter constraint variables to your personal
preferences e.g.

(setq afm-max-pixels 18)
(setq afm-min-pixels 10)
(setq afm-weight "medium")
(setq afm-slant "r")
(setq afm-width "normal")
(setq afm-charset "iso8859-1")

These may be accessed via m-x customize-group <ret> afm <ret>
