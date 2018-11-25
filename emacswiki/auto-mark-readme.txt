auto-mark push marks automatically when you change kind of command sequence.
For example, previous-line and next-line is motion comman, and
self-insert-command is edit (insert) command, so auto-mark will push
a mark automatically after C-n if you type `C-p C-p C-n f o o',
because `C-p C-p C-n' is motion command sequence and `f o o' is
edit command sequence.

auto-mark will regard a command as motion command if the command causes
to move a point, and will regard a command as edit command if the command causes
to change the buffer size.

In addition, you can specify a kind of command by adding a pair of COMMAND and CLASS
into `auto-mark-command-class-alist'. For example, if you want to make auto-mark to
regard `goto-line' command as a jump command not move command to push a mark automatically
when you goto-line even after move command sequence, add '(goto-line . jump) into the list
so that auto-mark can detect command sequence changes.

To use this package, please add following code into your .emacs:
(require 'auto-mark)
(global-auto-mark-mode 1)


TODO documentation
