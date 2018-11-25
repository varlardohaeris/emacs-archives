This mode is not stable. Do backup file if you visit important data.

Put this file into your load-path and the following into your ~/.emacs:
(add-to-list 'load-path "/path/to/lib")
(autoload 'tsv-mode "tsv-mode" "A mode to edit table like file" t)
(autoload 'tsv-normal-mode "tsv-mode" "A minor mode to edit table like file" t)

TODO:
1. add some column command, such as move, add, delete.
2. enable undo
3. enable formula (this may need more time)

_* Code:
