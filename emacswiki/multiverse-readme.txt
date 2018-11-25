Sometimes during programming I want to make experimental
modifications on a file and also want to be able to return to the
previous version if the modifications prove to be a dead end.

Version control systems make this easier, but often I don't yet
want to check in the current version, because it's not complete,
but want to experiment nevertheless.

In such cases I usually make a backup copy of the file, but it's
tedious, so I made this package which keeps multiple versions of
the current buffer in temporary files, so that the user can return
quickly to a previously saved version.
