 random-quote provides a simple mechanism to pick a random quote in a
 file. It is sufficient to put the file name containing the quotes in the
 `random-quote-file' variable. Next, at each invocation of
 `pick-random-quote', a string containing a random quote will be
 returned. The `random-quote-file' format has been kept as simpler as
 possible; at moment it simply consists in a quote per line.

Installation:

 Put this file on your Emacs-Lisp load path, then add one of the following
 to your ~/.emacs startup file.  You can load random-quote every time you
 start Emacs:

    (require 'random-quote)
