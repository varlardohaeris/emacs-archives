When one finds out about something it's useful if the information
is recorded somewhere. I usually use some file to dump everything
in it and use incremental search to find stuff. This is not
efficient, it is more useful to limit the search to a particular
context, so the the information can be retrieved easier. One can
keep separate note files, of course, but these tend to proliferate.

This package provides a very simple information manager mode where
information snippets can be tagged and retrieved using tags.

Each information snippet has a title and several tags. The title
begins with a * and tags are in the title after a / character:

  * some title /tag1 tag2 ...

Tags are separated with whitespaces.


tagger-mode activates the mode in a buffer.

tagger-show shows only the snippets having all the given tags or
everything if an empty string is given.


Note there is tab completion for tags both in the snippet title and
in the minibuffer.


Tested on Emacs 22.
