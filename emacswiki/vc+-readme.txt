   Extensions for `vc.el'.

Note: This code is quite OLD, and is LIKELY OBSOLETE NOW.  You
      might find it useful in some way to mine - or not. ;-)

-------------------------------------------------------------------

 See also the companion file `vc-.el'.
       `vc-.el' should be loaded before `vc.el'.
       `vc+.el' should be loaded after `vc.el'.

 All Dired buffers are now treated as if in `vc-dired-mode'.
 For example, you can use `C-x v v' to register marked files.

 New functions defined here:

   `vc-dired-relist-entry', `vc-dired-update', `vc-ediff',
   `vc-status-below', `vc-status-below-other-frame',
   `vc-status-below-other-window',`vc-status-here',
   `vc-status-here-other-frame', `vc-status-here-other-window'.

 New user option defined here: `vc-log-width'.

 Other variable defined here: `vc-last-dired-option'.


 ***** NOTE: The following functions defined in `vc.el'
             have been REDEFINED HERE:

   `vc-ensure-vc-buffer' - Treat Dired buffers like `vc-dired-mode'.
   `vc-finish-logentry' - 1. Uses `remove-windows-on'.
                          2. Doc string explains more.
                          3. Treats Dired like `vc-dired-mode'.
   `vc-log-mode' - 1. Doc string reflects new bindings.
                   2. `vc-comment-ring-index' is not local.
   `vc-next-action' - 1. Treats Dired buffers as `vc-dired-mode'.
                      2. Treats file registering like checking in:
                         `vc-start-entry' vs `vc-next-action-dired'.
                      3. Changes to log prompt and doc string.
   `vc-next-action-dired' - 1. Update all dired buffers.
                            2. `vc-dired-update-line' only if in
                               vc-dired buffer.
                            3. Redisplay only if < 2 files marked.
   `vc-next-action-on-file' - Calls `vc-register' with FILE arg.
   `vc-previous-comment' - 1. Better msg, with help on bindings.
                           2. Treat null `vc-comment-ring'.
   `vc-register' - 1. Lets `vc-next-action' register files too.
                      a. Added optional FILE argument.
                      b. Pass COMMENT arg to `vc-admin'.
                   2. Usable in Dired buffer too.
   `vc-rename-file' - 1. Can be called from Dired buffer.
                      2. Error if different directory.
                      3. Update buffer if Dired.
                      4. Added confirmation message at end.
   `vc-revert-buffer' - 1. Prefix arg => don't need confirmation.
                        2. Treats Dired buffers as `vc-dired-mode'.
   `vc-start-entry' - Lists bindings for previous comments in msg.


 The following binding is made here for vc-dired mode:

   `"'        `vc-ediff'


 This file should be loaded after loading the standard GNU file
 `vc.el'.  So, in your `~/.emacs' file, do this:
 (eval-after-load "vc" '(require 'vc+))
