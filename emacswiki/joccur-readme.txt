This package implements the functionality of "occur" but with some
modifications. The *JOccur* buffer will display the lines containing
the matching regexp with the original text properties.

I used the occur-mode code in replace.el(GNU Emacs 21.3.50.8) as the model
for this package. Indeed, a few helper functions are copied directly from
replace.el and renamed. The remaining helper functions I have reimplemented.
In addition, the actual "occur" engine has been modified extensively.

Comments and/or constructive criticism is always welcome.


Installation:

1. Place joccur.el in your emacs load-path
2. Add (require 'joccur) to your .emacs file


Usage:

M-x joccur regexp
