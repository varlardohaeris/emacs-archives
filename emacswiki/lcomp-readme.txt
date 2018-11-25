This package provides following features:

 - `lcomp-mode': make the completions buffer window disappear after
	use

  - `lcomp-keys-mode': add keybindings to the completions buffer

You want to use this package. put these lines into your ~/.emacs.

    (require 'lcomp)
    (lcomp-mode 1)
    (lcomp-keys-mode 1)

The completions buffer is usually only dismissed after completion
when it is created from minibuffer completion, but `lcomp-mode'
makes it get dismissed correctly from any buffer (e.g. shell, or by
calling `comint-dynamic-complete-filename').

Key Bindings:

`lcomp-mode' adds global keybindings if enabled:
   "M-v"	  -> lcomp-select-completion-window-or-original

`lcomp-keys-mode' adds keybindings to the completions buffer if enabled:
   "C-i"	  -> next-completion
   "M-C-i"	  -> previous-completion
   "f"	  -> next-completion
   "b"	  -> previous-completion
   "n"	  -> next-line
   "p"	  -> previous-line
   " "	  -> scroll-up
   [del]	  -> scroll-down
   [backspace] -> scroll-down
   "q"	  -> delete-completion-window
