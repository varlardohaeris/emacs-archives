Documentation:

* Description

This package is used to edit files produced by diff -u or
equivalent CVS diff.  You can use it to review your patch after you
have created it, to remove unwanted portions, to regenerate other
portions, reverse them, etc.  Only unified format patches are
supported.

* Installation

To install this package, add the following to your .emacs:

(setq auto-mode-alist (append '(("\\.diff$" . unidiff-mode))
  auto-mode-alist))

and make sure to load this source file.  To have it auto-loaded any
time unidiff-mode is requested:

(autoload 'unidiff-mode "/path/to/unidiff.el" nil t)

* Note

In the documentation, the word 'file' is used ambiguously,
referring sometimes to the actual patch file and some other to the
individual file-relative portions in the patch.
