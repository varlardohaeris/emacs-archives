Commentary:

This adds some faces to the *Article* buffer in Gnus. The main
purpose of this package is to display articles with a proportional
font. Hence the name "gnus-propfont".

Put the file into your load-path, compile it and insert the
following lines into your .gnus:

The defaults are according to *my* taste, so there's a good chance
that you won't like them.


(require 'gnus-propfont)
(add-hook 'gnus-article-prepare-hook 'gpf-add-faces)

You can customize this package via `M-x customize-group RET
gnus-propfont RET'.

The you can define a different face for each group. See the
documentation for the variable `gpf-groups-alist' for detaills.

The variables `gpf-exclude-header' `gpf-exclude-signature' controll
if the header resp. the signature should get a face, too.

It is possible to exclude certain parts of an article. See the
variable `gpf-excluded-parts-alist'. Currently a decent mechanism
exists only for lisp snipplets in Emacs related newsgroups.

Acknowledgments:
