A hook for C, C++ and Java modes that guesses the indentation
offset used in an existing C-like source code.

To install, add this file to one of your load directories,
byte-compile it and put the following at the very end of your
.emacs file:

   (require 'guess-offset)

When installed, guess-offset will briefly analyze every visited C
or C-like source code (C++ and Java, at the time) and make a
"guess" what indentation offset was used for creating this source.
If the guess is considered reliable, c-basic-offset is overridden
to reflect the proposed offset.

Guess-offset doesn't add anything to your Emacs environment, but it
removes something you won't miss - the hassle to manually deal with
source codes that are formatted with a different indentation
offset.  With guess-offset, Emacs will transparently adapt to
foreign indentation offsets.

It doesn't work perfectly for every possible source file.  In
particular, it won't work with source codes that use a single space
per level, and not with those that use varying indentation
depending on the outer construct.

Still, it will make dealing with a heap of source codes much
easier, and leave you no worse off with the remainder than before.
Remember, the c-basic-offset setting will only be tinkered with if
there is enough evidence that the guessed offset is the right one.

GuessOffset relies on heuristics and has a couple of variables you
can play with.  Please tune these settings if you are not satisfied
with the current behaviour.  If you think your settings are better
than the defaults, please send me a copy.

Note to users of other languages: the only reason guess-offset is
currently limited to C-like languages is that it can only deal with
C comments and expressions ("/*...*/" and "(...)", resp.).  It
shouldn't be too hard to extend this to work with other languages.
Please send a patch to the author if you do.
