Commentary:

These are some helper functions to use glimpse from within Emacs.
They contain:

* glimpse-dired: Conduct a glimpse search and display the files
found in a dired buffer.

* glimpse-find-file: Conduct a glimpse search and open one of the
files found

* glimpse and glimpse-in-files:  Run glimpse like grep.

`glimpse' and `glimpse-in-files' are from code posted by Peter
Breton <pbreton@i-kinetics.com> and are included here for
completeness.

Several variables are of potential interest.  `glimpse-switches'
are the default switches on the command line for `glimpse' and
`glimpse-in-files'.  `glimpse-dired-switches' are the switches used
for `glimpse-find-file' and `glimpse-dired'.  The switches given
are what I use... the code is probably not robust enough to handle
other switches.  Let me know what you like to use and I'll make it
work.

This code is very beta.  The code works fine for me, but it doesn't
handle all cases people might use.  Please let me know if you'd
like it to do something else and I'll try to fix it.  Patches are
also welcome.
