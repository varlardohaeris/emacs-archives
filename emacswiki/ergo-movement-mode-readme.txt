Ergo Movement mode is a global minor mode which defines ergonomic
keybindings for cursor movement. See the function documentation
string below for more information.

The movement keys are inspired by Xah Lee's Ergoemacs keybindings:
http://xahlee.org/emacs/ergonomic_emacs_keybinding.html

Installation:

Put this file somewhere in your load-path. You can add an autoload
function to your ~/.emacs file

    (autoload 'ergo-movement-mode "ergo-movement-mode"
      "Ergonomic keybindings for cursor movement" 'interactive)

Or if you want to turn on the mode automatically when Emacs is
started put these lines in your ~/.emacs file:

    (require 'ergo-movement-mode)
    (ergo-movement-mode 1)
