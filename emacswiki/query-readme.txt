This library implements a small number of functions that can be used
by other programs to prompt the user for answers, with easily
customizable default values.

The user can specify (globally) how much confirmation is required
for a function in this library to accept the user's answer.

The "confirm" function is a simple yes-or-no type of query.
The "query-string" function can be used to request a string answer.
The "query-character" function can be used to prompt for a single
character.

INSTALLATION

Put this file somewhere in your load-path and byte-compile it.
Other libraries that wish to use this one should put a

(require 'query)

statement at the top of the file.

OPTIONALLY, you can put the following lines in your .emacs:

(require 'query)
(setq confirm-level 'single)
(setq allow-confirm-defaults t)
(fset 'yes-or-no-p 'confirm)
(fset 'y-or-n-p 'confirm)

This will make all yes-or-no prompting accept a single 'y' or 'n'
for an answer.  See the documentation for the "confirm-level"
variable for other options.

LCD Archive Entry:
query|Martin Boyer|gamin@ireq-robot.hydro.qc.ca|
Functions to query the user|
01-Mar-1994|1.0|~/packages/query.el.Z|

Changes from version 1.0 to version 1.0.1 (Claus Brunzema):
- Comment changes to enable upload to http://www.emacswiki.org.
- Typo fix.
