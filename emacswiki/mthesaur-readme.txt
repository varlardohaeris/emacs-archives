This code was inspired by thesaurus.el by Ray Nickson.

This code depends on the following file:

The Project Gutenberg Etext of Moby Thesaurus II by Grady Ward
Filename: mthesaur.txt
URL: ftp://ibiblio.org/pub/docs/books/gutenberg/etext02/mthes10.zip

Availability:

This code is available at <http://www.emacswiki.org/elisp/mthesaur.el>




Introduction
------------

This package provides a way to perform a thesaurus search on a word or
phrase using the awesome Project Gutenberg Etext of Moby Thesaurus II
by Grady Ward.

To use mthesaur, insert in your ~/.emacs:

   (require 'mthesaur)

Or:

   (autoload 'mthesaur-search "mthesaur"
     "Thesaurus lookup of a word or phrase." t)
   (autoload 'mthesaur-search-append "mthesaur"
     "Thesaurus lookup of a word or phrase, append results." t)

Optionally followed by assigning key sequences to the mthesaur
functions:

   (global-set-key "\C-ct" 'mthesaur-search)
   (global-set-key "\C-c\C-t" 'mthesaur-search-append)

In the detailed instructions below, I assume that you have assigned
mthesaur-search to `C-c t' and mthesaur-search-append to 'C-c C-t'.

Be sure to modify the mthesaur-file variable to point to where you've
put your copy of the mthesaur.txt file.

For good performance, be sure to byte-compile mthesaur.el, e.g.

   M-x byte-compile-file <give the path to mthesaur.el when prompted>

Or open the mthesaur.el file and run "Byte-Compile This File" from the
"Emacs-Lisp" menu.  Either way, this will generate mthesaur.elc, which
will be loaded instead of mthesaur.el.

mthesaur.el was tested with GNU Emacs 21.3.1.


Using mthesaur
--------------

To lookup the word at the cursor, or to lookup the phrase in the
marked region, type:

   C-c t

To be prompted to enter a word or phrase to be looked up, type:

   C-u C-c t

The normal history manipulation commands (M-p, M-n, etc.) work as
expected at the minibuffer prompt.  Even the words/phrases that were
added via `C-c t' will be in the history list.

The 'C-c C-t' key sequence behaves identically to 'C-c t' except the
search results are appended to the end of the previous search results
instead of overwriting them.
