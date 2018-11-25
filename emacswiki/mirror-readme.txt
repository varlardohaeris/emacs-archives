Provides a facility to update a (possibly remote) copy of the
current buffer either automatically (by setting
`mirror-update-on-save') or on demand (via the function
`mirror-update-this-file').  Just how this happens is determined by
a series of buffer local variables.  Since it is very lightweight
and makes few assumptions about the remote mirror files and hosts
it can handle certain situations that `ange-ftp' or `tramp' cannot
(yet).

Installation:

Place the following somewhere in your .emacs file

 (require 'mirror)

and set one or more of the following in the local variables section
of a file you wish to mirror (see the bottom of this file for
example).

`mirror-file-path': set this to the full path (including filename)
to the mirror file.  Note that the mirror file need not have the
same name as the local file.  This can be useful for getting around
suffix conventions on different machines (eg .html versus .HTM).

`mirror-file-host': set this to the name of the remote host on
which the mirror file resides.  If this is nil (the default) then
the file is assumed to reside on the same file system and hence the
emacs lisp function `copy-file' is used to perform the update.  If
this variable is non-nil then a remote connection to the host is
established using `mirror-sftp-command'.

`mirror-sftp-command': set this to the shell command used to
establish a remote connection to `mirror-file-host'. The default is
"sftp".  This remote session will persist throughout your emacs
session and the same session will be used to update any mirror
files on that host.  You may also have to set
`mirror-password-prompt-regexp' `mirror-login-prompt-regexp' and
`mirror-prompt-regexp' to get this to work.

`mirror-time-to-connection': set this to the amount of time you are
willing to wait for a connection to the remote host to be
established.  The default is 10 seconds which is usually enough,
but you can set it higher for connections you expect to be
especially slow.

`mirror-update-command': set this to the command that should be
passed to `mirror-sftp-command' whenever a the mirror file is to be
updated. The default is "put". This command must take two
arguments: the full path to the current file and the full path to
the remote file.

`mirror-update-on-save': if this is non-nil then the mirror file
will automatically be updated whenever the buffer is saved. The
default is nil. This facility is most useful when you wish to edit
files on a remote system where using `tramp' or `ange-ftp' are too
slow or just don't work, for example, editing files on a Windows
2000 server (not running CygWin) from a UNIX machine (this was the
original motivation for the package).
