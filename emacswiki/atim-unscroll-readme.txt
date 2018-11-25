The position of cursor and buffer look are automatically remembered
when a scrolling command is invoked (pg-down/pg-up, mouse wheel
scrolling, scroll-bar movement)

This information is stored in a ring; you can go up and down this
ring restoring you position and text visibility before the scrolling
command with

atim-unscroll-down (bound to [M-down])
atim-unscroll-up   (bound to [M-up])

these commands travel the ring to older (..-down) and
newer (..-up) stored points.



Installation:

1) Put atim-unscroll.el to your load-path.

2) Add the following to your ~/.emacs startup file.

  (require 'atim-unscroll)
  (atim-unscroll-global-mode) ;; to enable in all buffers


3) Optional step(s):

a) To enable atim-unscroll in just some types
of buffers, comment out the `atim-unscroll-global-mode' and put
atim-unscroll-mode in some major-mode hook, like:

(add-hook 'c-mode-common-hook '(lambda () (atim-unscroll-mode)))

b) Alternatively, do use `atim-unscroll-global-mode' and create
*exceptions* using the `atim-unscroll-dont-activate' local
variable, like:

(add-hook 'c-mode-common-hook
         '(lambda () (setq atim-unscroll-dont-activate t)))
