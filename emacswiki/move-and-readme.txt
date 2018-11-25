I just saw a blog where there was a complaint about the
complexity of key-bindings for killing some piece of text which was
contrasted with the simplicity of vi where one prepends a motion
keysequence with `d' in order to delete the text passed over by the
motion command. Independently I have been annoyed by the difficulty
of deleting text without putting it into the kill ring. The blog
suggested using `M-d' as a prefix command to various deleting
commands. Here I propose a better idea. Given some keysequence
`key' that defines a motion command, the keysequence `M-d key'
deletes without saving the region passed over; then the keysequence
`M-k key' kills the region passed over and finally the keysequence
`M-c key' copies the region passed over. This file also provides a
general way to combine a motion command with a region command to
obtain a command that applies the region command to the region
passed over by the motion command.

There is some difficulty to doing this consistently because emacs
is not consistent in its treatment of motion commands; some of them
have an optional numeric argument whilst some of them have a forced
numeric argument. Also the sequence `M-2 M-b' is two not one
keysequence. So to kill two words backward one must type `M-2kb'
rather than `M-k2b'. To set it up the other way would be more
complex though possible. What do others think should be done?
