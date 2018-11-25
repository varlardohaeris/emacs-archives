Use `edi-mode' to edit raw edi files. Here is how to do that:

(add-to-list 'auto-mode-alist '("\\.edi" . edi-mode))

Or, if you are > Emacs version 21, you can recognize them
like:

(add-to-list 'magic-mode-alist '("^ISA" . edi-mode))

Key Bindings:
  C-c C-r -- convert a single line EDI file to a multi-line
             edi file (Readable)
  C-c C-e -- covert a multi-line edi file to a single line
             edi file (Non-readable)
  C-c C-c -- query for, and count the give edi segment


History
1.0.4    2016-11-26
 * Added variables for redefining keywords. Ideally they
   should be read from the UNA segment. (troelskn)

 * Added font-lock on all keywords. (troelskn)

1.0.3    2007-03-09
 * Added nothing more than a reference to setting up via
   magic-mode-alist. Many EDI files do not have a common
   extension.

1.0.2    2005-12-20
 * Highlights Segment Id's at begining of line as well as after a
   segment terminator (Christian Plate)

 * Counts segments correctly while in EDI format (one line only).

1.0.1    2005-02-08
 * Updates for (S)XEmacs (Steve Youngs <steve@sxemacs.org>)

1.0.0    2004-12-10
 * Initial Creation
