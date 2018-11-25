Recently, I began to learn how to write gtk applications. GLib, gtk
and gnome function is sometimes to long to type which exactly like
elisp symbols, and I like use PC-lisp-complete-symbol to write
elisp. So I began to write some elisp function to extract functions
from gtk, GLib and gnome header files.

Put this file into your load-path and the following into your ~/.emacs:
  (require 'clibpc)
  (add-hook 'c-mode-hook
            (lambda ()
              (define-key c-mode-map "\t" 'clibpc-complete-function)
              (eldoc-mode 1)
              (set (make-local-variable 'eldoc-documentation-function)
                   'clibpc-eldoc-function))))
