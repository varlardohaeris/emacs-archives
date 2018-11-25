Sometimes it happens that you are in isearch mode - having entered
a larger regular expression - and notice that there are much more
hits than you expected.  In that case, you usually want to replace
them automatically.  One option to do that is setting
`query-replace-interactive', but this will be set for all
searches.  Another problem in these cases is that the regexp has
grown so fast that it is hard to see if the replacement does the
right things.  So in these cases, use ireplace.

When using regexp or normal isearch, hit C-RET or M-% to switch to
replace mode. This will ask you for a replacement-string and call
`query-replace' or `query-replace-regexp' afterwards. A prefix arg
to these keystrokes is passed to the `query-replace(-regexp)'
function.

When using regexp isearch, hit C-RET or M-% twice to switch to
ireplace mode. This will ask for a replacement as well, but the
first 10 matches and their replacements are shown in a separate
buffer while you type. Use a prefix arg to the second C-RET or M-%
to specify the number of results, or a simple C-u arg after you
typed a text to see the results but not update them.

In both cases, you can hit `M-e' to re-edit the search string.

When using regexp isearch, hit M-RET or C-M-% to switch to
`individual replace' mode. This will ask you for a replacement
string for each distinct match of the given regexp and then call
`query-replace-regexp-eval' to perform the replace.
