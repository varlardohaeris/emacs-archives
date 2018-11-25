This is a hacked version of the Free Software Foundation's
`underline.el', by Theron Tlax <thorne@timbral.net>.  It implements
two kinds of underlining:

The first is the old form of underlining consisting of prefixing
each character with "_\^h".  The entry point `underline-region'
performs such underlining on a region.  The entry point
`ununderline-region' removes it.  You can control the way
`underline-region' works by setting the variable
`underline-exclusion-function' to point to a function that returns
t if a given character should not be underlined.  By default it is
set to cause the old behavior (`underline-classic'), underlining by
word, skipping all whitespace.  Another possibility causes
`underline-region' to underline continuously including spaces
between words and sentences (`underline-continuous').

Perhaps most usefully, you can point `underline-exclusion-function'
to `underline-by-sentence' (or write your own).

The other underlining method uses font-lock to do underlining in
the buffer.  You can turn this feature on or off with
`toggle-underline-by-font-lock'.  A prefix arg will prompt for the
delimiter (default is an underscore character: `_').  It takes a
character or a string and underlines any occurrences of strings it
finds that are delimited by that string.  So, if you use the
underscore character (`_') then any thing like _this_ would be
fontified with the underscores invisible and the word underlined.
This of course is only visible in Emacs, but the delimiter remains
in the actual file, which allows you to do search-and-replace
operations later based on your delimiter, perhaps changing _this_
to LaTeX:

   \emph{this}

or troff:

   .u this
