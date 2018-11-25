I write a number of commands to search documents of symbols and
display them. It is hard to bind a common key for those command.
One command is need. So I write it to provide one command that
almost does everything.

Woman Note: The woman-topic-all-completions is create using
 woman-file-name which will prompt for the file name. So you
 may have to M-x woman before active woman.

Dependencies:
 no extra libraries is required

Installation:
Put this file into your load-path and the following into your ~/.emacs:
  (require 'help-dwim)
