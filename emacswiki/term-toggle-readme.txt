 Most of code is got from
 http://user.it.uu.se/~mic/shell-toggle.el
 And thanks to the Author:Mikael Sjödin <mic@docs.uu.se>
 The only diffenerce to shell-toggle.el
 is this will use `term' command instead of `shell'

Installation:

o Place this file in a directory in your 'load-path.
o Put the following in your .emacs file:
  (autoload 'term-toggle "term-toggle"
   "Toggles between the *terminal* buffer and whatever buffer you are editing."
   t)
  (autoload 'term-toggle-cd "term-toggle"
   "Pops up a shell-buffer and insert a \"cd <file-dir>\" command." t)
  (global-set-key [M-f1] 'term-toggle)
  (global-set-key [C-f1] 'term-toggle-cd)
o Restart your Emacs.  To use term-toggle just hit M-f1 or C-f1
