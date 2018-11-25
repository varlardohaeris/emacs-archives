Multi-Shell Manager

Here, have below command you can use:

     `multi-shell-new'                create a new shell buffer.
     `multi-shell-next'               switch to next shell buffer.
     `multi-shell-prev'               switch to previous shell buffer.
     `multi-shell-current-directory'  create a new shell with current-directory.

     choose your like key bind above command. ;)

This package extension `shell-mode' with below features:

1 ->
     Can create or navigation shell buffers quickly.

2 ->
     Close buffer when type command `exit' in shell buffer.

3 ->
     Interrupt sub-process before kill shell buffer forcible.

4 ->
     Revert window configuration before completion window popup.


Installation:

Put multi-shell.el to your load-path.
The load-path is usually ~/elisp/.
It's set in your ~/.emacs like this:
(add-to-list 'load-path (expand-file-name "~/elisp"))

And the following to your ~/.emacs startup file.

(require 'multi-shell)

And setup program that `multi-shell-command' will need:

(setq multi-shell-command "/bin/bash")

     or setup like me "/bin/zsh" ;)
