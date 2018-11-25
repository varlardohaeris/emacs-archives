 The main functionality provided by this file is in the
 `perl-read-library' function, which reads the name of a perl
 module from the minibuffer with completion. We also provide
 `perl-library-path', which turns the name of a perl library into a
 path on the system.

 We also expose a `perldoc' function, which is a wrapper around
 `cperl-perldoc' that uses `perl-read-library', as well as
 `perl-find-library', which reads the name of a perl library from
 the minibuffer and finds the file corresponding to it on disk.
