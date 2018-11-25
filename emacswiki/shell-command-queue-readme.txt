 This package provides a means to add shell commands to a queue for
 "sequentual asynchronous execution".  This means that each command
 is run asynchronously from Emacs (so that Emacs is not locked up),
 but successive commands are not executed before the previous ones
 have finished.

 If you want to try it out, load this file and evaluate the
 following expressions:

  (shell-command-queue-clear)                ;; Clear the queue
  (shell-command-queue-add "s5" "sleep 5")   ;; Add a simple sleep command
  (shell-command-queue-add "s10" "sleep 10") ;; And another one
  (shell-command-queue-run)                  ;; Start executing

 If a command fails the execution stops and leaves the failing
 command as the current command (so that you can look at what it
 was and what was wrong with it).  You can try this by adding the
 following simple command somewhere in the middle of the queue:

  (shell-command-queue-add "test of a failing command" "exit 5")

 The output from each command, if any, ends up in the buffer named
 by `shell-command-queue-output-buffer'.  A convenience command,
 `shell-command-queue-show-output' is available for switching to
 this buffer.
