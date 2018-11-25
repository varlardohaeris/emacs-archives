Dynamic abbreviation expansion in the middle of a word.  For example, if
the cursor is on the letter 'b' in "foo_bar" and the word "foo_quux_bar" is
in the current or other buffers, "quux_" will be inserted.  You can cycle
through all expansions just like dabbrev.  This also works at the beginning
and end of words, although at the end it's not as smart as regular dabbrev;
e.g. you can't hit space and have it continue to add more text.

03 Dec 2008 -- v1.0
               Initial release
