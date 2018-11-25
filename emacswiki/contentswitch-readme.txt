Since the advent of timid.el, anything.el and similar tools one can
quickly open a recent file by typing a part of its name without
knowing in which directory the file is located. It's very
convenient, but what if one doesn't remember the name either, only
some of the file contents?

This package provides a command `contentswitch' which allows the
user to switch to a buffer or an unopened file by typing a part of
its content, instead of its name, and selecting a file from the
result list with the cursor keys and ENTER. (By default the
substring is also tested on the file/buffer name, but the purists
can disable it with an option.) Note that the search works for
files only if you (as all other sane people) use recentf, savehist
or a similar package.

Matching on the file content can be useful if one can come up with
a fairly unique string from the file, but what if the string is not
unique enough? Usually, the first thing popping into one's mind is
the direct context of what he was working on. E.g. I began to
implement that for loop for counting things. "for" is not a really
unique word, so typing it probably results in lots of unwanted
matches. To avoid this the package can list those matches first
which have the given string within the context of the current
position of point in the file. See option
`contentswitch-context-bias' about tuning this behavior. The
context search requires saveplace.el, otherwise point location
cannot be determined for unopened files.

See additional configuration options below.


Tested on Emacs 22
