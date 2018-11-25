Usage:
(require 'mail-field)
(add-hook 'mail-mode-hook '(lambda ()
     (define-key mail-mode-map [C-tab] 'mail-next-field)
     (define-key mail-mode-map [S-C-tab] 'mail-previous-field)))
(add-hook 'message-mode-hook '(lambda ()
     (define-key message-mode-map [C-tab] 'mail-next-field)
     (define-key message-mode-map [S-C-tab] 'mail-previous-field)))
