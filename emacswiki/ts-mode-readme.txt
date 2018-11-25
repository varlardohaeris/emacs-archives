Installation:

To install just drop this file into a directory on your load-path and
byte-compile it.  To set up Emacs to automatically edit files ending in ".ts"
using ts-mode add the following to your ~/.emacs file (GNU Emacs) or
~/.xemacs/init.el file (XEmacs):
   (setq auto-mode-alist (cons '("\\.ts$" . ts-mode) auto-mode-alist))
   (autoload 'ts-mode "ts-mode" "TypoScript file editing mode." t)

Description:

This is a major mode for editing TypoScript input files.  It is developed to
support syntax highlighting, indentation and folding of blocks.

This file is *NOT* part of GNU Emacs.
