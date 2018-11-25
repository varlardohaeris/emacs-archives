Files with binary format are better opened in appropriate
applications. This regards for an instance Openoffice-files or
Starcalc-files. If this package is loaded the find-file command of
emacs starts appropriate application for files with names matching
entries in application-caller-list (we refer to such a file as an
application document). The association of the applications with the
file names is implemented rather low-level.  Because of that the
application ist started whenever one opens an application document
via find-file. That works with C-x C-f as well as with dired-find.

The file buffer corresponding to the application document is
associated with the application process.  If the application ends
the buffer is killed.
