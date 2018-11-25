This module changes the look of Buffers list and Buffer menu.

*** Modification status ***

Column M is extended to distinguish a buffer that has been modified and
saved from a buffer that has never been modified.
The new meaning of column M is:
  ` '  untouched:  buffer has never been modified
  `.'  saved:      buffer has been modified and saved (previously ` ')
  `*'  modified:   buffer has modifications that are not saved

This is somewhat useful in deciding which buffers to drop or keep.
It uses `buffer-undo-list' to detected if there has been modifications.

*** Compacting ***

If flag `list-buffers-compact' is non-nil the text in the Size and Mode
columns are truncated. The Size indicates kilo-chars (k) rather than chars
(but only if the combined length of buffer name and size would be to big)
and the Mode string is truncated to 12 chars.

This somewhat reduces the risk that the File columns is pushed to the right
by a long buffer name. (But it can still happen if the buffer name and the
mode name is quite long.)

*** Colors ***

The M, R, Size and Mode columns are shown in `list-buffers-status-face'.
This makes it somewhat easier to read the columns, even if they are
skewed by very long buffer names.

In the M column, `*' is shown in `list-buffers-modified-face' if the
buffer is visiting a file.
This makes it somewhat easier to find buffers that needs to be saved,
as opposed to eg *scratch* and *Messages* that need not.

(Font-lock-mode is not used. If font-lock-mode is enabled in "*Buffer List*"
it will override this colorization.)

*** Character syntax ***

The syntax table is changed so that `.' (and other chars that usually
occurs in file names) has symbol syntax. Thus it is somewhat easier to
copy and paste filenames (and buffer names) by double-clicking on them.

*** Buffer menu ***

In the Buffers menu the modification column is changed to distinguish a
buffer that has been modified and saved from a buffer that has never been
modified. It is the same change as for column M in Buffers list:
  ` '  untouched:  buffer has never been modified
  `.'  saved:      buffer has been modified and saved (previously ` ')
  `*'  modified:   buffer has modifications that has not been saved

*** Usage examples ***

  (require 'fix-buffers-list)
  (setq list-buffers-compact t)
  (setq list-buffers-modified-face 'bold)

  ; or

  (require 'fix-buffers-list)
  (require 'font-lock)  ; define more faces (colours)
  (setq list-buffers-compact t)
  (setq list-buffers-status-face   'font-lock-string-face)
  (setq list-buffers-modified-face 'font-lock-warning-face)

  ; or

  (require 'fix-buffers-list)
  (require 'font-lock)  ; define more faces (colours)
  (setq list-buffers-compact t)
  (setq list-buffers-status-face   'font-lock-builtin-face)
  (setq list-buffers-modified-face  nil)

  ; etc

*** Compatibility ***

Works with Emacs-19, Emacs-20 and Emacs-21.2.
Does not work with Emacs-22 and XEmacs.

This module redefines many existing standard functions that builds the
buffer list and the buffer menu. It does not affect `mouse-select-buffer'
(msb.el).

History:
 1.5 david 2010-04-27 Update email
 1.4 david 2010-04-18 Incompatibility comment about Emacs-22
 1.3 david 2006-05-16 Colour special buffer names
 1.2 david 2006-02-21 Compact mode name with list-buffers-mode-alist
 1.1 david 2006-02-02 Rename list-buffers-status-face. Compact home dir to ~
 1.0 david 2004-11-26 First version
