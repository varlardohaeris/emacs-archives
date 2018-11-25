Installation:
put nc.el/nc.elc in your load-path
add (autoload 'nc "nc" "Emulate MS-DOG file shell" t) to _emacs/.emacs
type `M-x nc'

The descriptions moved to the docstring for the mode.


Things that could be done but havent been:
- directory tree
- update view of backup-file after saving a buffer from nc
  (see nc-update-current-file and nc-local-set-keys)
(- undo function)
