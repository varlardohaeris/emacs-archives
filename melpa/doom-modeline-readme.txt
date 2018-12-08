This package offers a modern modeline them which is extracted from DOOM Emacs
(https://github.com/hlissner/doom-emacs/tree/master/modules/ui/doom-modeline).
It's also integrated into Centaur Emacs (https://github.com/seagle0128/.emacs.d).

The doom-modeline was designed for minimalism and fast, and offers:
- A match count panel (for anzu, iedit, multiple-cursors, symbol-overlay,
  evil-search and evil-substitute)
- An indicator for recording a macro
- Local python/ruby version in the major-mode
- A customizable mode-line height (see doom-modeline-height)
- An error/warning count segment for flycheck
- A workspace number segment for eyebrowse
- A perspective name segment for persp-mode
- A window number segment for winum and window-numbering
- An indicator for evil state
- An indicator for god state
- An indicator for ryo-modal state
- An indicator for xah-fly-keys state
- An indicator for remote host
- An indicator for current input method
- An indicator for LSP state
- Truncated file names, file icon, buffer state and project name in buffer
  information segment, which is compatible with projectile or project

Installation:
From melpa, `M-x package-install RET doom-modeline RET`.
In `init.el`,
(require 'doom-modeline)
(doom-modeline-init)
or
(use-package doom-modeline
  :ensure t
  :defer t
  :hook (after-init . doom-modeline-init))
