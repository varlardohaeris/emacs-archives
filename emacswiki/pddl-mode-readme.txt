Purpose:

To provide a pleasant mode to browse and edit PDDL files.
It provides syntax highlighting with automatic indentation,
templates, auto-completion, and a Declaration imenu which
list all the actions and the problems in the current file.


This mode supports full PDDL 2.2

Installation:

Put in your ~/.emacs:
     (add-to-list 'load-path "/lib/emacs/PDDL-mode")
     (require 'PDDL-mode)

Version 0.100: PDDL-mode released
History:

This mode was written by Surendra Singhi in February 2005.
Special thanks also goes to Stefan Monnier <monnier@iro.umontreal.ca> for
helping me with various parts of the code
If you have any problems or suggestions or patches specific to the mode
please contact the author via email.
