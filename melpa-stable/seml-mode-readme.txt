Sample configuration with [[https://github.com/conao3/leaf.el][leaf.el]]

(leaf real-auto-save
  :ensure t
  :custom ((real-auto-save-interval . 0.3))
  :hook (find-file-hook . real-auto-save-mode))

(leaf seml-mode
  :config (require 'seml-mode)
  :custom ((seml-live-refresh-interval . 0.35)))
