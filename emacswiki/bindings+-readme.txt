Commentary:

 This lets you click `mouse-1' on a minor-mode lighter in the
 mode-line, to pop up the corresponding minor-mode menu, if there
 is one.

 This is equivalent to what `mouse-1' does when you click the
 major-mode lighter.  Each minor-mode lighter brings up its own
 menu. If a minor mode has no menu-bar menu, then `mouse-1'
 displays a message stating that.

 Example: Minor mode Icicles has lighter "Icy" in the mode line,
 and it has a menu-bar menu `Icicles'. Whether or not the menu-bar
 is showing, if you click the lighter "Icy" then the `Icicles' menu
 pops up.

 Emacs 23+:

  I added the functionality provided here to vanilla GNU Emacs, so
  this library is not strictly needed for Emacs 23.  The code here
  just provides slightly better feedback than the vanilla version.

 To use this library, put the following in your init file
 (~/.emacs):

   (require 'bindings+)
