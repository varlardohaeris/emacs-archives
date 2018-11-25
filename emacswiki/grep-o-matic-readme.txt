This package lets the user launch a search, with a single key
combination, for the word under the cursor, in the current
repository, or the current directory, or in the set of currently
open files.

Repository-wide search should work out-of-the-box for vc backends
known to Emacs, and may be further enhanced with the
repository-root library. Git grep may optionally be used instead of
grep in git repositories.

Works nicely in combination with grep-a-lot.

Installation:

1. Put this file in a directory that is a member of load-path, and
   byte-compile it (e.g. with `M-x byte-compile-file') for better
   performance.
2. Add the following to your ~/.emacs:
   (require 'grep-o-matic)
3. Customize grep-o-matic with `M-x customize-group grep-o-matic'

Default Key Bindings:

M-] M-/         Search for current word in current repository
M-] M-.         Search for current word in current directory (recursive)
M-] M-,         Search for current word in currently opened files
                Prefix these with C-u to edit the search regexp
