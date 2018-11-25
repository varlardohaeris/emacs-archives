It occured to me that I rarely type single characters without doing
something afterwards (like saving the file), so I thought it could
be used for convenient single-key command execution.

If a self-insert-command is typed after a non self-insert-command
then singlebind waits for a short while (singlebind-command-delay)
and if there are no new input events then it checks if there is a
command assigned to the key which invoked the self-insert-command.

If a command is found then the effect of the self-insert-command is
canceled and the command is invoked. Here's how to assign a command
to a key:

   (push '(?g . grep) singlebind-command-map)


In read-only buffers singlebind-command-delay is not used and the
bound command is invoked immediately.


Thanks to Thorsten Bonow for making it work on XEmacs.
