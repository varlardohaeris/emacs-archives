This package provides helm interface for flyspell-correct package.

Points of interest are `flyspell-correct-wrapper',
`flyspell-correct-previous' and `flyspell-correct-next'.

Example usage:

  (require 'flyspell-correct-helm)
  (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-wrapper)
