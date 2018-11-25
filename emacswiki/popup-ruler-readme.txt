Displays a ruler at (point) for measuring and positioning text.

Three different rulers can be displayed depending on prefix arg.
One measures from the left most column, one from point and one
reaches over a delimiter such as quotes to measure text within.

(load "popup-ruler") and see the help strings for
`popup-ruler' and `popup-ruler-vertical'

This ruler was inspired by the one in fortran-mode but code-wise
bears no resemblance.

Installation:

Put popup-ruler.el on your load path, add a load command to .emacs and
map the main routines to convenient keystrokes.  For example:

(require 'popup-ruler)
(global-set-key [f9]    'popup-ruler)
(global-set-key [S-f9]  'popup-ruler-vertical)

Please report any bugs!
