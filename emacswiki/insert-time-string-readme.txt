Usage:
M-x insert-time-string
C-u M-x insert-time-string

Customizations:

Key binding: (global-set-key (kbd "C-c t") 'insert-time-string)

Date/Time format:
 (setq insert-time-string-format-alist
      (cons '("pseudo-iso" . "%Y-%m-%d %T") insert-time-string-format-alist))

Voilà!
C-c t pseudo-iso RET

If that's too much typing:
(setq insert-time-string-default-format "pseudo-iso")
C-c t RET
