file-props.el provides a way to "tag" or "label" your files.
Actually, the functionality is generic and supports adding any type
of property to your files.  Right now "tags" and a "comment" can be
added.

After having added this meta data you can use it to find files in
different ways.  Provided is a command to mark all files having a
certain tag in a dired buffer.

Installation:

Place this file in your `load-path'.

Put this in your .emacs file:

  (require 'file-props)

If you want to activate display of comment and tags for Dired, put
this in your .emacs as well:

  (file-props-dired-activate-display)

If you want to disable the display temporarily, do M-x
file-props-dired-deactivate-display

To setup convenient Dired keybindings, put this:

 (file-props-dired-setup-keybindings)
