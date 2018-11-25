Emacs' dired mode displays directory contents in columns, and column
widths are determined by "ls -l". When a subset of the entries are
modified (e.g. name or permissions changed), only those entries are
redisplayed, and "ls -l" uses different column widths than for the entire
directory. The modified content doesn't line up and looks bad. This
module fixes the problem with dired hooks.

This module works by capturing the column widths when the directory
listing is first produced, and applying those column widths whenever
content is modified.

ls's "-R" option presents a special problem: each subdirectory is listed
separately with its own column widths. When "-R" is used, this module
reformats the listing so all sections have the same column width. That
takes longer, and if you're using "-R" it already takes longer. Sorry. I
don't use "-R", so it's unlikely that I'll improve on this.

Limitations:

  * Sometimes a field's width grows beyond the column's width. For
    example, an entry's size or inode may grow one digit larger than all
    the others, and this is not discovered until that one entry is
    redrawn. Result: while not illegible, it doesn't quite fit correctly.
    Work-around: revert dired buffer.

  * Unanticipated ls options may insert unanticipated columns before the
    date column, and confuse this module. If that happens, modify the
    "just" list in dired-capture-column-widths-subdir and add another
    option test.


Installation:

Add to .emacs:

  (add-hook 'dired-load-hook
            (lambda ()
              (load "dired-column-widths.el") ))

You may already load other dired extensions. Brief and incomplete testing
indicates that this module does not interact with the others, and has no
load order requirements.

Also suggested is to add this to the dired-load-hook lambda:

               ;; Replace leading inode and size detection pattern to
               ;; match metric modifiers output by modern "ls -s".
              (setq dired-re-inode-size "\\(?:[0-9. \t]+[kKMGTPEZY]? \\)?")


Configuration:

 None. You either love it or hate it.
