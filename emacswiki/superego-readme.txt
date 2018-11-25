* What is it?

This package implements a simple minor mode that enables
occurrences of your name (or other arbitrary text) to be
highlighted in the current buffer.

By default your full name, your login name, your e-mail address and
your last and first name individually are highlighted.  You can
alter this behavior by setting the superego-regexp variable
(either in your .emacs or via M-x customize-variable).

* Usage

To use it, make sure to load superego in your .emacs or have it
auto-loaded on request:

    (autoload 'superego-mode "/path/to/superego.el" nil t)

Then for each mode you want, add a line like the following:

    (add-hook 'text-mode-hook 'superego-mode)
    (add-hook 'rmail-mode-hook 'superego-mode)

You can also enable the mode interactively in a specific buffer,
via M-x superego-mode.

* Thanks

Benjamin Drieu, author of egocentric.el, which inspired me to write
this package.
