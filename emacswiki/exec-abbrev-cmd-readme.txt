This file  includes the command  `exec-abbrev-cmd' which lets you  execute a
command by  giving it in an  abbreviated form, where  the abbreviation takes
the first character of each word in the command name.  For example "g" is an
abbreviation  for   the  command  `gnus',   "eb"  is  an   abbreviation  for
`emms-browser' and "omm" is an abbreviation for `outline-minor-mode'.  If an
abbrev contains hyphens the number of  words in the command match the number
of hyphens  plus one.   So the  abbrev "o-m-m" is  equivalent to  the abbrev
"omm".   Of course  it is  possible,  that an  abbreviation matches  several
commands, e.g. "g" matches not only  `gnus' but `grep', `gdb' and some more.
In such cases you will be queried, which command to use.

When you hit TAB on the exec-abbrev-cmd prompt, you'll fall back to the
normal behavior of `execute-extended-command' with the current minibuffer
contents as initial input.

To have this  functionality quickly accessible you might want  to bind it to
some key.  That's what I use:

    (add-to-list 'load-path "~/elisp") ;; Where is exec-abbrev-cmd.el?
    (require 'exec-abbrev-cmd)         ;; Load it.
    (global-set-key (kbd "C-x x") 'exec-abbrev-cmd)
    ;; Since you can fall back to normal `execute-extended-command' behavior
    ;; with TAB, its perfectly ok to bind `exec-abbrev-cmd' to `M-x'.
    ;; (global-set-key (kbd "M-x") 'exec-abbrev-cmd)

Now you'll say,  "Wow, what a nice feature!", but  it's even getting better.
Let's say  you often invoke  `customize-face' with `C-x  x cf RET'  and then
choosing from the completion  list between `copy-file' and `customize-face'.
Always `copy-file' is selected  first, because it's lexicographically before
`customize-face'.     As    a    solution    to   this    problem    there's
`exec-abbrev-cmd-mode', a global minor  mode that does bookkeeping how often
you invoke  a command with `exec-abbrev-cmd',  so that the  list of commands
you have  to choose from is  sorted by the frequency  of command invokation.
After a while in most cases `C-x x <abbrev> RET RET' will do what you want.

If you want to enable this feature put this in your ~/.emacs:

    (exec-abbrev-cmd-mode 1)

Have fun!
