A major-mode that uses original PHP lexer tokens for syntax coloring and indentation
making it easier to spot errors in syntax.

Also includes full support for PSR-1 and PSR-2 indentation and imenu.
Improved syntax table in comparison with old PHP major-mode.

For flycheck support run `(phps-mode-flycheck-setup)'.

For asynchronous lexer run: `(setq phps-mode-async-process t)'

For asynchronous lexer via `async.el' instead of threads run: `(phps-mode-async-process-using-async-el t)'

Please see README.md from the same repository for extended documentation.