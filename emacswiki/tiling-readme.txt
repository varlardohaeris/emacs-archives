As the display geting larger and larger, "Tiling" is getting more and more
popular. Emacs is tiling windows within the frame for sure. However, by
default, it does not provide an easy way to change a set of preset layouts
like what Xmonad and tmux did. This package is trying to provide one solution

To install: download this file into to you load-path and "(require 'tiling)"
in your init file.

Strongly recommand you use this package together with windmove, winner-mode
and buffermove. The last one is not part of Emacs yet. Beneath is my
configuration:

Windows related operations
Split & Resize
(define-key global-map (kbd "C-x |") 'split-window-horizontally)
(define-key global-map (kbd "C-x _") 'split-window-vertically)
(define-key global-map (kbd "C-{") 'shrink-window-horizontally)
(define-key global-map (kbd "C-}") 'enlarge-window-horizontally)
(define-key global-map (kbd "C-^") 'enlarge-window)
Navgating: Windmove uses C-<up> etc.
(define-key global-map (kbd "C-<up>"   ) 'windmove-up)
(define-key global-map (kbd "C-<down>" ) 'windmove-down)
(define-key global-map (kbd "C-<right>") 'windmove-right)
(define-key global-map (kbd "C-<left>" ) 'windmove-left)
Swap buffers: M-<up> etc.
(define-key global-map (kbd "M-<up>"   ) 'buf-move-up)
(define-key global-map (kbd "M-<down>" ) 'buf-move-down)
(define-key global-map (kbd "M-<right>") 'buf-move-right)
(define-key global-map (kbd "M-<left>" ) 'buf-move-left)
Tile
(define-key global-map (kbd "C-\\") 'tiling-cycle) ; accepts prefix number
(define-key global-map (kbd "C-M-<up>") 'tiling-tile-up)
(define-key global-map (kbd "C-M-<down>") 'tiling-tile-down)
(define-key global-map (kbd "C-M-<right>") 'tiling-tile-right)
(define-key global-map (kbd "C-M-<left>") 'tiling-tile-left)
Another type of representation of same keys, in case your terminal doesn't
recognize above key-binding. Tip: C-h k C-up etc. to see into what your
terminal tranlated the key sequence.
(define-key global-map (kbd "M-[ a"     ) 'windmove-up)
(define-key global-map (kbd "M-[ b"     ) 'windmove-down)
(define-key global-map (kbd "M-[ c"     ) 'windmove-right)
(define-key global-map (kbd "M-[ d"     ) 'windmove-left)
(define-key global-map (kbd "ESC <up>"   ) 'buf-move-up)
(define-key global-map (kbd "ESC <down>" ) 'buf-move-down)
(define-key global-map (kbd "ESC <right>") 'buf-move-right)
(define-key global-map (kbd "ESC <left>" ) 'buf-move-left)
(define-key global-map (kbd "ESC M-[ a" ) 'tiling-tile-up)
(define-key global-map (kbd "ESC M-[ b" ) 'tiling-tile-down)
(define-key global-map (kbd "ESC M-[ c" ) 'tiling-tile-right)
(define-key global-map (kbd "ESC M-[ d" ) 'tiling-tile-left)
