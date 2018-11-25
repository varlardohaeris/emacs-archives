   Enhancements of standard library `ls-lisp.el'.

 If you use MS Windows, MS-DOS, or MacOS, then you will likely want
 to use library `ls-lisp.el' plus this library, to use an Emacs
 Lisp only definition of `insert-directory'.

 `ls-lisp+.el' loads libraries `ls-lisp.el' and `files+.el'.  Both
 `files+.el' and `ls-lisp+.el' redefine `insert-directory' so that
 the second header line includes the number of files and
 directories in the directory.  Files `.' and `..' are excluded
 from the count, but all other directories listed are included.

 The second header line thus becomes this, in Emacs 22:

   files 276 space used  27359 available 56238272

 or this, in Emacs 20 and 21:

   files 276 total 27359

 This library also lets you use wildcards in the file names in an
 explicit cons arg to `dired'.  It thus provides a bug fix for
 Emacs bug #7027.

 This library also provides a fix for bug #2801 for Emacs 21 and 22
 for the case where switches `F' and `R' are both provided.  This
 is fixed in vanilla Emacs 23.


 ***** NOTE: The following functions defined in `ls-lisp.el' have
             been REDEFINED HERE:

 `ls-lisp--insert-directory' - Different 2nd header line.
 `insert-directory' - If wildcard, set FILE to `default-directory'
                      if no dir component.  Include number of files
                      in 2nd header line.
 `ls-lisp-insert-directory' - If FILE nil, use `default-directory'.
                              Do nothing if FILE is "".
