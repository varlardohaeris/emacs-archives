Some extension for auto-complete-mode.

Below are commands you can use:

`ac-source-gtags'
     Provide a completion for C or C++.
     You need install `gtags' first.

`ac-source-c++'
     Provide a completion keyword for C++.

`ac-source-haskell'
     Provide a completion for Haskell.
     You need install `GHC' and `hoogle' first.

About how to use this package, please see:

http://www.emacswiki.org/emacs/download/init-auto-complete.el


Installation:

Put auto-complete-extension.el to your load-path.
The load-path is usually ~/elisp/.
It's set in your ~/.emacs like this:
(add-to-list 'load-path (expand-file-name "~/elisp"))

And the following to your ~/.emacs startup file.

(require 'auto-complete-extension)

No need more.
