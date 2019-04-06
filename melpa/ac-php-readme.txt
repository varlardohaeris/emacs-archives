Auto Completion source for PHP.  Known to work on Linux and macOS systems.
For more info and examples see URL `https://github.com/xcwen/ac-php' .

Thanks to:
- auto-complete-clang
- auto-java-complete (ac-php-remove-unnecessary-items-4-complete-method)
- rtags (ac-php-location-stack-index)

Usage:  Put this package in your Emacs Lisp path (eg. site-lisp) and add to
your .emacs file:

  (add-hook 'php-mode-hook
            '(lambda ()
               (auto-complete-mode t)
               (require 'ac-php)
               (setq ac-sources '(ac-source-php))
               (yas-global-mode 1)

               (define-key php-mode-map (kbd "C-]")
                 'ac-php-find-symbol-at-point)
               (define-key php-mode-map (kbd "C-t")
                 'ac-php-location-stack-back)))

Many options available under Help:Customize
Options specific to ac-php are in
  Convenience/Completion/Auto Complete

Bugs: Bug tracking is currently handled using the GitHub issue tracker
(see URL `https://github.com/xcwen/ac-php/issues')
