Place this file somewhere in your `load-path', and add:

(require 'wcy-swbuffer)
(global-set-key (kbd "<C-tab>") 'wcy-switch-buffer-forward)
(global-set-key (kbd "<C-S-kp-tab>") 'wcy-switch-buffer-backward)
then you can use <C-tab> and <C-S-kp-tab> to switch buffer.
