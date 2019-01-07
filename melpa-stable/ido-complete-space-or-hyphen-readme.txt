The default behavior of ido SPACE key will try to insert SPACE if it makes
sence (a.k.a, the comman part of all matches contains SPACE). Howerver,
when ido is used to complete lisp functions or variables, like what smex
does, HYPHEN is used as separator. This extension for ido inserts SPACE or
HYPHEN whenever which one makes sence, just like what built-in M-x does.

Example:

Choises: "ido-foo-bar", "ido-space" "idotest"

After you type "i", then SPACE key. The input text is completed to "ido-" and
HYPHEN is inserted for you.

However if the choises are "ido-foo-bar", "ido-space" and "ido test", the input
text is completed to "ido", type SPACE again will insert SPACE.

Usage

    (require 'ido-complete-space-or-hyphen)
    (ido-mode t)

Changes

-   1.1 (2013-02-27)

    -  Add `ido-complete-space-or-hyphen--insert-space' to allow user type
       SPACE twice to insert SPCE.
