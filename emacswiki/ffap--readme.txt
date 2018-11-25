 Extensions to library `ffap.el'.

 To use this library, add this to your initialization file
 (~/.emacs or ~/_emacs):

     (require 'ffap-) ; Load this library.

 You should explicitly load *only* `ffap-.el', not also `ffap.el'
 (this will automatically load `ffap.el').

 This library redefines variable `ffap-bindings' as a user option
 (not just an internal variable).  The default key bindings in
 variable `ffap-bindings' are also changed.  No bindings are
 created by this library however; to create the default bindings,
 you must call command `ffap-bindings' or evaluate (ffap-bindings).

 By default, this library inhibits the behavior of ffap in Dired
 buffers, because I usually do *not* want to find the file where
 the cursor is.  Instead, I want to use completion to provide the
 file name.  This inhibition is done by setting option
 `ffap-inhibit-ffap-flag' to t in Dired buffers:

   (add-hook 'dired-mode-hook 'ffap-inhibit-here)

 If you do *not* want to inhibit ffap in Dired, then do this after
 loading this library:

   (remove-hook 'dired-mode-hook 'ffap-inhibit-here)


 User options defined here:

   `ffap-inhibit-ffap-flag'.

 Functions defined here:

   `ffap-inhibit-ffap-here'.

 Internal variables defined here:

   `ffap-max-region-length'.


 ***** NOTE: The following variables defined in `ffap.el' have
             been REDEFINED HERE:

 `ffap-bindings' - Use defcustom, not defvar.
                   Change mouse bindings.


 ***** NOTE: The following functions defined in `ffap.el' have
             been REDEFINED HERE:

 `ffap-guesser' - Respect `ffap-inhibit-ffap-flag'.

 `ffap-read-file-or-url-internal' - Bug fix:
    If the cursor is on a URL when you use `find-file-at-point',
    and you delete the URL in the minibuffer and then try to use
    completion (to a file name), you get an error with message
    "Wrong type argument: stringp, nil."
