Go to char.

This package is simple forward or backward character.

Below are commands you can use:

`go-to-char-forward'         goto char forward.
`go-to-char-backward'        goto char backward.
`go-to-char-forward-word'    goto char forward with word.
`go-to-char-backward-word'   goto char backward with word.


Installation:

Put go-to-char.el to your load-path.
The load-path is usually ~/elisp/.
It's set in your ~/.emacs like this:
(add-to-list 'load-path (expand-file-name "~/elisp"))

And the following to your ~/.emacs startup file.

(require 'go-to-char)

No need more.
