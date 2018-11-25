Put this file into your load-path and the following into your ~/.emacs:
(require 'sql-completion)
(setq sql-interactive-mode-hook
      (lambda ()
        (define-key sql-interactive-mode-map "\t" 'comint-dynamic-complete)
        (sql-mysql-completion-init)))
To save time of building database schema, add sql-mysql-schema to
desktop-globals-to-save:
(require 'desktop)
(add-to-list 'desktop-globals-to-save 'sql-mysql-schema)
