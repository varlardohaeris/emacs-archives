   Extensions to `diff.el' for Emacs 21 and later.
   The frame is resized to fit buffer "*Diff*".

 Library `diff.el' changed significantly from Emacs 20 to Emacs 21.
 For extensions to `diff.el' that work with Emacs 20, see library
 `diff+20.el'.

 For extensions to `diff' highlighting in Emacs 21 and later, see
 library `diff-mode-.el'.


 ***** NOTE: The following function defined in `diff.el' has
             been REDEFINED HERE:

   `diff-sentinel' - Works with multiple Emacs versions.
                     Fits frame to *Diff* buffer.


 This file should be loaded *after* loading the standard GNU file
 `diff.el'.  So, in your `~/.emacs' file, do this:

   (eval-after-load "diff" '(require 'diff+))
