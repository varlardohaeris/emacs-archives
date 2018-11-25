This package lets you edit the results of a 'grep' by pressing "\C-c\C-e",
then write the results back to the files by pressing "\C-c\C-s" (or
aborting by pressing "\C-c\C-e" again).

The variable `grep-ed-save-after-changes' determines if the changes will be
written back automatically or if you have to do it manually.  The variable
`grep-ed-unload-new-buffers-after-changes' determines if any new buffers
created to make changes will be automatically unloaded.

Since files you want to modify could be read-only (e.g. because they are in
a revision control system), the `grep-ed-make-file-writable-function' and
`grep-ed-make-file-read-only-function' variables can be pointed to your own
functions that take one argument FILENAME and, say, check the file out then
back in.

09 Jan 2009 -- v1.0
               Initial release
