Start with M-x globallff and type in any substring of any path on
your system to display the matching files. The displayed list is
updated dynamically as you type more characters or delete some.

Needs an up-to-date locate database for file name searching.

Since the search is based on locate you can use any globbing
characters allowed by the locate command.

You can move up/down the list with the cursor keys (I know these
bindings are not very Emacsian, but I happen to like them) and
select a file to open with Enter.

You can quit with C-g.

See the variable `globalff-map' for further bindings.


XEmacs port was done by Stefan Kamphausen.
Customize support contributed by Lennart Borgman and Stefan Kamphausen.
Camel case support added by Eyal Erez.
