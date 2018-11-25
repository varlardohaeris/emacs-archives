Extra features for John Wiegley's timeclock package:

(1) Implement history lists for `timeclock-ask-for-project', and
`timeclock-ask-for-reason' (for people who prefer to go [up] rather
than [tab]).  Calls to `timeclock-completing-read' now give the
first element of the corresponding history list as the default. Our
history mechanism also skips duplicates and null strings.

(2) Provide `timeclock-query-in' -- a useful function to add to the
end of your .emacs file.  Extended `timeclock-query-out' to ask for
a reason as well.

(3) Facility for managing multiple timelog files (corresponding to
distinct jobs -- very useful if you have more than one employer who
wants to know how you've been spending your time).  See the
documentation for the variables `timeclock-directory',
`timeclock-default-log', `timeclock-multiple-jobs', and
`timeclock-job-list'.

(4) Make safe versions of `timeclock-in', `timeclock-out', and
`timeclock-change' that don't screw up your log files if you do a
keyboard quit part way through:-)

(5) Make `timeclock-visit-timelog' ignore `require-final-newline'
which can sometimes bugger up you files.

(6) Make `timeclock-read-moment' skip non-moment lines (nice if you
like to beautify your timelog files).

(7) Extended comment facillity controlled by the boolean variable
`timeclock-multiline-comments'.  If this is set then you will be
prompted for a multiline comment whenever you clock out or change
project.  Comments in your time log file are simply lines beginning
with white space (see (6) above). Project comments are inserted
after the "clock-in" time stamp but before the "clock-out" time
stamp, and are indented to the same column as the project name

(8) Implements periodic project confirmation for those of us who
often forget to change projects at the right time. This really
helps to keep the timelog files accurate. See the documentation for
the variable `timeclock-query-interval' and the functions
`timeclock-query-project-on', `timeclock-query-project-off', and
`timeclock-toggle-query-project'.

(9) Alternative `timeclock-update-modeline' function which tells
you which project (and job) you are clocked into and how long you
have been working on that project for.

(10) Provide a convenience function `timeclock-setup-keys' to bind
interactive timeclock functions to a "C-x t-" prefix.

Bugs

Odd behavior using timeclock `timeclock-query-in' in .emacs file.
A query during startup causes GNU Emacs to wait for keyboard input
finishes).  I think this is one for the Emacs Developers.

Installation:

Plcae the following somewhere in your .emacs files

(require 'timeclock-x)
(timeclock-modeline-display 1) ;; if you want modline display
(timeclock-initialize)

and see the timeclock doco:-)
