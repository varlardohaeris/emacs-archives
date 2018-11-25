To enable lsp-p4 include the following lisp code in init.el after
loading lsp-mode

   (with-eval-after-load 'lsp-mode
     (require 'lsp-p4)
     (add-hook 'p4-mode-hook #'lsp-p4-enable)

See `lsp-p4lsd-executable' to customize the path to p4lsd.
