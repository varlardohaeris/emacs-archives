The ediff-trees package is a simple frontend to the emacs' ediff
package to allow a simpler comparison of two similar directory
trees.

I wrote this package because I often need to compare two different
versions of the same directory tree and ediff-directories is not
very helpful in this case.  Specially when the directory trees to
compare are deep and only a few files have changed.
Typically, that occurs when I create a copy of some project
directory tree either to make some experiments myself or to send to
someone else that will return a modified directory tree to me
later.  (Yes, I heard of version control systems, and I use them
regularly.  Yet, for several reasons, sometimes that is not an
option.)

Later, when I want to integrate the modified directory tree with
the original tree, I want to see the differences to the original
version, so that I may decide whether to accept the changes or not.
This is where this package kicks in...

To use it, just call `ediff-trees', which will ask for two
directories to compare.  Usually, I give the original directory as
the first one and the modified directory as the second one.

ediff-trees recursively descends both directories, collecting the
pairs of files that are worth "comparing": either files that
changed, or that appear in one of the two directory trees but not
in the other.  Then, it shows the first "change" using ediff.

In fact, ediff-trees either uses ediff to compare a file with its
changed version, or simply opens a file that occurs in only one of
the trees.

The user can then navigate backward and forward in the set of
changes by using `ediff-trees-examine-next' and
`ediff-trees-examine-previous', respectively.  These functions move
from one change (quiting the current ediff session or killing the
current file buffer) to another.  Therefore, by repeatedly using
these functions we can go through all the changes.  I usually use
some global bindings for these functions.  Something like this:

  (global-set-key (kbd "s-SPC") 'ediff-trees-examine-next)
  (global-set-key (kbd "S-s-SPC") 'ediff-trees-examine-previous)
  (global-set-key (kbd "C-s-SPC") 'ediff-trees-examine-next-regexp)
  (global-set-key (kbd "C-S-s-SPC") 'ediff-trees-examine-previous-regexp))

The `ediff-trees-examine-next-regexp' and
`ediff-trees-examine-previous-regexp' skip over the list of changes
to a file with a filename that matches a given regexp.

This package allows for some customization.  Please, see the
ediff-trees group under customize.

Finally, to deal with small changes in the white space I often find
it useful to configure ediff like this:

  (setq ediff-diff-options "-w")
  (setq-default ediff-ignore-similar-regions t)
