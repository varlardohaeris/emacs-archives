A single-file personal wiki for quick structured note taking.

There is only one markup character: `<word> becomes a link
to a page automatically.

If you want spaces in the link the close it with an other `

      `word1 word2 word3`

Links are case-insensitive. The link `main page` is reserved for
the main page.

To search the whole wiki you can use isearch as usual.


Put this in your .emacs file to turn on the wiki mode automaticaly
for .sw (spartan wiki) files:

    (require 'spartan-wiki)
    (add-to-list 'auto-mode-alist '("\\.sw$" . spartan-mode))



Tested on Emacs 22
