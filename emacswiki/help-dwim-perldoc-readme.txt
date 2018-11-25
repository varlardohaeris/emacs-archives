Put this file into your load-path and the following into your ~/.emacs:
  (require 'help-dwim-perldoc)
  (add-hook 'cperl-mode-hook
            (lambda () (help-dwim-active-type 'perldoc)))
