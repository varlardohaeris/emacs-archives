Sets up C-mode with support for BC. Any improvements needed?

For autoload, ass the following lines to your .emacs:
(autoload 'bc-mode "bc-mode.el" "bc-mode" t 'nil)
(add-to-list 'auto-mode-alist '(".bc\\'" . bc-mode))
(add-to-list 'interpreter-mode-alist '("bc" . bc-mode))

Kumar Appaiah akumar_NOSPAM@ee.iitm.ac.in
Date: 27th June, 2005
