Some extensions for rect.el

Have some nice functions:
     `execute-command-with-region' can execute command with region
     or rectangle, and then return result.

     `execute-command-with-region-replace' can execute command with
     region or rectangle and replace original one.

     `execute-command-with-region-kill' like `execute-command-with-region-replace',
     different is use kill action instead replace action.

     `mark-rectangle-to-end' will mark rectangle to the longest column
     in current rectangle.


Installation:

Put rect-extension.el to your load-path.
The load-path is usually ~/elisp/.
It's set in your ~/.emacs like this:
(add-to-list 'load-path (expand-file-name "~/elisp"))

And the following to your ~/.emacs startup file.

(require 'rect-extension)

No need more.
