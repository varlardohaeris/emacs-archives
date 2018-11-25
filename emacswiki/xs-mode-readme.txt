This elisp is for fix font-lock highlighting when use c-mode as
major mode in XS files. The indentation of XS code is too difficult
for me. I have to give it up.

Put this file into your load-path and the following into your ~/.emacs:
  (require 'xs-mode)
Or for autoload:
  (autoload 'xs-mode "xs-mode" "Major mode for XS files" t)
  (add-to-list 'auto-mode-alist '("\\.xs$" . xs-mode))
