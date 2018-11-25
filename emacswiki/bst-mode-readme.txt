Integrated with bst.el by Nelson H. F. Beebe <beebe@math.utah.edu>

Installation:

To use this package, put the following line in your .emacs:

   (require 'bst-mode)

or, if you want to load this package only when necessary

   (add-to-list 'auto-mode-alist '("\\.bst$" . bst-mode))
   (autoload 'bst-mode "bst-mode" "BibTeX-style major mode." t)
