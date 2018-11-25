Introduction
------------

This package provides routines to compare string version and to convert
string version into an integer list.

versions was tested with GNU Emacs 22.0.50.1, 23 and 24.

I don't know if it is still compatible with XEmacs.

It provides the following functions:

`version-to-list'		convert a version string into an integer list.

`version-list-<'		return t if integer list L1 is lesser than L2.

`version-list-='		return t if integer list L1 is equal to L2.

`version-list-<='		return t if integer list L1 is lesser than or
				equal to L2.

`version='			return t if version V1 is equal to V2.

`version<'			return t if version V1 is lesser than V2.

`version<='			return t if version V1 is lesser than or equal
				to V2.


Usage
-----

To use versions, insert in your Emacs Lisp code:

   (require 'versions)

So, you can compare versions in Emacs Lisp code like:

   (and (version< other-version "6.6pre4")
        (error "`my-pack' requires `other' package v6.6pre4 or later"))


Acknowledgments
---------------

Thanks to Chong Yidong <cyd@stupidchicken.com> for extending
`version-to-list' to handle versions like "10.3d".

Thanks to Eli Zaretskii <eliz@gnu.org> for extending valid syntax for
version string in `version-regexp-alist'.

Thanks to Kim F. Storm <storm@cua.dk> for:
  * fixing `version-list-not-zero'.
  * allowing space as separator before non-numeric part, e.g. "1.0 alpha" in
    `version-regexp-alist'.
  * interpreting ".X.Y" version as "0.X.Y" version in `version-to-list'.

Thanks to Sébastien Kirche <sebastien.kirche@free.fr> for fixing typos and
testing.
