Provide an error display function to show Flycheck errors inline, directly
below their location in the buffer.

# Setup

Enable the global minor mode after Flycheck:

(with-eval-after-load 'flycheck
  (global-flycheck-inline-mode))

Or enable the local minor mode for all flycheck-mode buffers:

(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'turn-on-flycheck-inline))
