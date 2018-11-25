Commentary:

Bitcoin donations gratefully accepted: 1FgnGFwRES9MGzoieRDWLmLkjqMT3AsjQF

This library provides some macros for extracting keyword args from lists.
It can be helpful in situations where normal cl-defun arglist handling is not enough.



Installation:

Put keyword-arg-macros.el in a directory in your load-path, e.g. ~/.emacs.d/
You can add a directory to your load-path with the following line in ~/.emacs
(add-to-list 'load-path (expand-file-name "~/elisp"))
where ~/elisp is the directory you want to add
(you don't need to do this for ~/.emacs.d - it's added by default).

Add the following to your ~/.emacs startup file.

(require 'keyword-arg-macros)

Require
