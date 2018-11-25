Notify popup for rcirc

This extension use `notify-send' for notify.
So make you have install `notify-send' in your system.


Installation:

Copy rcirc-notify+.el to your load-path and add to your ~/.emacs

 (require 'rcirc-notify+)

Rcirc will notify you automatically when have a message is reach, blow is open
rcirc notify switcher:
 (setq rcirc-notify+-open t)

Little tips:
 Function `rcirc-notify+-jump-last-message-channel' can jump last channel that
 message notify you.
 And feel free to binding it to you like. ^_^
