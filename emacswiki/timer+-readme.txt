   Extensions to `timer.el'.


 ***** NOTE: The following function defined in `timer.el' has
             been REDEFINED HERE:

 `cancel-function-timers' -
    This now uses `completing-read' in the interactive spec, with,
    as default, `symbol-nearest-point'.


 This file should be loaded after loading the standard GNU file
 `timer.el'.  So, in your `~/.emacs' file, do this:
 (eval-after-load "timer" '(require 'timer+))
