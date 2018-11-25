To change the case of the current word with `Shift-F3', put
the following in your .emacs file:

(global-set-key (quote [S-f3]) 'fdlcap-change-case-current-word)

The order of change is lowercase, to capitalized, to
uppercase and then back to lowercase.  To do reverse the
order of operations, type `C-u' before hitting `Shift-F3'.

To change the case of the following word(s) forward with
Ctrl-Shift-F3, add the following in your .emacs file:

(global-set-key (quote [S-C-f3]) 'fdlcap-change-case-word)

Successively hitting `Ctrl-Shift-F3' will change the case of
the same number of words from the previous execution.

There currently is no way to reverse the order of this latter
command.  The ability to easily repeat the command should
take care of that.  The command is just an aggregate of the
Emacs commands `M-l', `M-c' and `M-u' -- the functions
`downcase-word', `capitalize-word' and `upcase-word'
respectively.

History

Written on 16 December, 2007 in South Burlington, Vermont, USA.

Proposal and code posted by Maverick Woo to the Emacs Wiki on 7
November, 2007.
