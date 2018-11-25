outline-wiki-mode is an extended outline mode. The only addition to
the normal outline capabilities is a link syntax to files. Every
consecutive range or characters without whitespace between braces
is treated as a link to a file.

Braces where chosen because brackets are often used for array
indices in some languages, so this might prove annoying. Braces are
annoying in TeX files, though.

The usefulness of such file links were proven by emacs-wiki. This
mode tries to be as useful as emacs-wiki without most of the more
arcane features of that mode. To ease the creation of links --
emacs-wiki allowed CamelCase links --, we provide a C-c C-x
shortcut to turn the current word into a link.
