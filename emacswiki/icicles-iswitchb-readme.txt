Integration of [[Icicles]] and [[IswitchBuffers]].

If you find that the file you are after is not in a buffer, you can
press C-x C-f to immediately drop into `find-file' in iswitchb.
But because `icicle-find-file' hijacks `iswitchb-find-file',
this feature is not usable when `icicle-mode' is enabled.
This package fixes this problem.

You can show *Completion* buffer in iswitchb by pressing TAB.
This package enhances this feature.
Pressing TAB enters `icicle-buffer'.
Then you can cycle and narrow candidates.

Installation:

(require 'icicles)
(require 'icicles-iswitchb)
(iswitchb-default-keybindings)
(icy-mode)

In Emacs 22 or later, you can use `iswitchb-mode' instead of
`iswitchb-default-keybindings'.  The order of loading the libraries
is unimportant, as is the order of activating the modes.
