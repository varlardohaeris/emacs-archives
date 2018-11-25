Installation instructions:

1. Put lnk.el somewhere in your load-path.
2. Put shortcut.vbs in an appropriate location (with lnk.el,
with other VBS scripts, etc).
3. Add the following lines to your .emacs file:

(require 'lnk)
(setq lnk-script FILENAME-OF-SHORTCUT)

To dereference shortcuts without asking permission do:

(setq lnk-always-follow-shortcuts t)
