   Extensions to `ring.el'.

 The code in this library is part of GNU Emacs 23 and later, so
 this library is useful only for releases prior to Emacs 23.

 Main new functions here:

   `ring-convert-sequence-to-ring', `ring-insert+extend',
   `ring-remove+insert+extend', `ring-member', `ring-next',
   `ring-previous'.


 This file should be loaded after loading the standard GNU file
 `ring.el'.  So, in your `~/.emacs' file, do this:
 (eval-after-load "ring" '(progn (require 'ring+))
