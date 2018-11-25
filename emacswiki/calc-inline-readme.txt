Matlab-like calc.

Main commands:

calc-inline (evaluate one line)

and

calc-inline-region (evaluate active region)

Bind this two function to the keys of your choice.

See doc of calc-inline-mode for comments of the structure of calc sheets.

Changes:
2011-02-08: calc-inline-region also works for the whole buffer (see doc of calc-inline-region).

2014-06-04: calc-inline-region: Work on active region even if called non-interactively with b=e=nil.
 Add function calc-inline-starter-re used in calc-inline and calc-inline-region for identification of calc-inline lines/regions.
 Therewith also out-commented regions are recognized by calc-inline-region.

2014-06-05: Rename calc-inline to calc-inline-line and redefine calc-inline as calc-inline-region if region is active and calc-inline-line else.
