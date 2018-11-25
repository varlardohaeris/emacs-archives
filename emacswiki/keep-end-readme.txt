Put this file into your load-path and the following into your ~/.emacs:
  (require 'keep-end)

use it for some interactive program, such as *SQL*, *R*:
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (keep-end-watch-this (current-buffer))))
(add-hook 'inferior-ess-mode-hook
          (lambda ()
            (keep-end-watch-this (current-buffer))))
