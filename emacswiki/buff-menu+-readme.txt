   Extensions to `buff-menu.el', including: new bindings, faces,
   and menus; selective column display; and directional column
   sorting.

   NOTE: Emacs Dev rewrote `buff-menu.el' for Emacs 24.2, so that
         it uses `tabulated-list-mode'.  I have not yet updated
         `buff-menu+.el' to accommodate this vanilla rewrite, and I
         do not know when I might get around to doing that.

         If you want to use `buff-menu+.el' with Emacs 24.2 or
         later, then you can download the Emacs 23 or Emacs 24.1
         version of `buff-menu.el' and put that in your `load-path'
         in such a way that it shadows the Emacs 24.2+ version.
         You can get the Emacs 23.4 version here, for instance
         (combine the URL into a single line):

           http://bzr.savannah.gnu.org/lh/emacs/emacs-23/download/
            head:/buffmenu.el-20091113204419-o5vbwnq5f7feedwu-197/buff-menu.el

         Sorry for the inconvenience.

   Note: By default, the buffer menu is shown in a different
         window.  If you prefer to show it in the current window,
         then just do this:

         (add-to-list 'same-window-buffer-names "*Buffer List*")

 Faces defined here:

   `buffer-menu-headings', `buffer-menu-current-buffer',
   `buffer-menu-directory-buffer', `buffer-menu-flagged-buffer',
   `buffer-menu-marked-buffer', `buffer-menu-star-buffer',
   `buffer-menu-view-mark', `buffer-menu-delete-mark',
   `buffer-menu-save-mark', `buffer-menu-modified-mark',
   `buffer-menu-read-only-mark', `buffer-menu-buffer-name',
   `buffer-menu-mode', `buffer-menu-size', `buffer-menu-time',
   `buffer-menu-file-name'.

 User options defined here (Emacs 22+):

   `Buffer-menu-file-flag', `Buffer-menu-mode-flag',
   `Buffer-menu-time-flag', `Buffer-menu-time-format'.

 Commands defined here:

   `Buffer-menu-decrease-max-buffer+size' (Emacs 22+),
   `Buffer-menu-delete-flagged',
   `Buffer-menu-increase-max-buffer+size' (Emacs 22+),
   `Buffer-menu-mouse-3-menu', `Buffer-menu-mouse-delete',
   `Buffer-menu-mouse-execute', `Buffer-menu-mouse-modified',
   `Buffer-menu-mouse-other-window', `Buffer-menu-mouse-save',
   `Buffer-menu-mouse-unmark', `Buffer-menu-toggle-file-column'
   (Emacs 22+), `Buffer-menu-toggle-mode-column' (Emacs 22+),
   `Buffer-menu-toggle-time-column' (Emacs 22+),
   `Buffer-menu-toggle-time-format' (Emacs 22+).

 Internal variables defined here:

   `buffer-menu-buffer-name',
   `Buffer-menu-buffer+size-computed-width',
   `buffer-menu-current-buffer', `buffer-menu-directory-buffer',
   `buffer-menu-flagged-buffer', `buffer-menu-marked-buffer',
   `buffer-menu-star-buffer', `buffer-menu-delete-mark',
   `buffer-menu-file-name', `buffer-menu-font-lock-keywords',
   `buffer-menu-headings', `buffer-menu-mode',
   `buffer-menu-modified-mark', `buffer-menu-read-only-mark',
   `buffer-menu-save-mark', `buffer-menu-size', `buffer-menu-time',
   `buffer-menu-view-mark'.

 Other functions defined here:

   `Buffer-menu-fontify-and-adjust-frame',
   `buffer-menu-nb-marked-in-mode-name',
   `buffer-menu-set-default-value'.


 ***** NOTE: The following user option (variable) defined in
             `buff-menu.el' has been REDEFINED HERE:

 `Buffer-menu-sort-column' - A user option now. Numeric, default=1.


 ***** NOTE: The following hook defined in `buff-menu.el'
             has been REDEFINED HERE:

 `Buffer-menu-mode-hook' (aka `buffer-menu-mode-hook') -
    Fontify buffer and fits its frame.
    Add number of marked and flagged lines to mode in mode line.


 ***** NOTE: The following functions defined in `buff-menu.el'
             have been REDEFINED HERE:

 `buffer-menu' -
    1. Different help message.
    2. Prefix ARG =< 0 now means list (all) buffers alphabetically.
       (It used to mean the same as ARG > 0.)
       Prefix ARG >= 0 means list just file buffers.
    3. Use pop-to-buffer instead of switch-to-buffer.
 `Buffer-menu-beginning' - Protected with `boundp' for Emacs 20.
 `Buffer-menu-buffer+size' - Use computed width for Buffer + Size.
 `Buffer-menu-execute' - Deletes windows (frame) when kills buffer.
 `Buffer-menu-make-sort-button' -
    1. If same column as last sort, flip direction of sort.
    2. Column header face indicates sort direction.
    3. CRM is indicated by COLUMN = 1, not by nil COLUMN.
 `Buffer-menu-mode' -
    1. Doc string reflects new bindings.
    2. mouse-face on whole line, not just buffer name.
 `Buffer-menu-select' - When Buffer Menu is `window-dedicated-p',
                        uses `pop-to-buffer' to display.
 `Buffer-menu-sort' -
    1. Allow negative COLUMN. Allow COLUMN = 1 or -1.
    2. When COLUMN = `Buffer-menu-sort-column', then flip that.
    3. Added message at end indicating the kind of sort.
 `list-buffers-noselect' - Use longest buffer name+size to indent.
                         - Change sort direction if same column.
                         - Add sort buttons for CRM and Time also.
                         - Sort test is different: no sort for CRM.
                         - Go to bob if `desired-point' undefined.

 In your init file (`~/.emacs') file, do this:

   (require 'buff-menu+)

 NOTE:

 1. This file MUST be saved with encoding UTF-8 or equivalent,
    because it contains an em-dash character.

 2. If you byte-compile this using a version of Emacs prior to 23,
    and you use the byte-compiled file with Emacs 23 or later, then
    some keys, such as `q', will not be defined in the buffer list.
    (So byte-compile it using Emacs 23 or later.)

 3. Starting with Emacs 24.3, Emacs development changed
    `buff-menu.el' so that it is based on `tabulated-list' mode.
    Unfortunately, that breaks the `buff-menu+.el' enhancements.  I
    have not had the time to update `buff-menu+.el' for
    compatibility with Emacs 24.3 and later.  If you want to use
    `buff-menu+.el' with Emacs 24.3 or later, you can download the
    Emacs 23 version of `buff-menu.el' and put that in your
    `load-path'.  You will lose no features if you do that: Emacs
    24.3 and later add no enhancements to `buff-menu.el' - they
    just base it on `tabulated-list.el'.  You can download Emacs 23
    `buff-menu.el' here: http://ftp.gnu.org/gnu/emacs/ or here:
    http://www.gnu.org/prep/ftp.html.  That version will work fine
    with Emacs 24.3 and later and with `buff-menu+.el'.  I might
    eventually get around to updating `buff-menu+.el' to
    accommodate the `buff-menu.el' change, but it is not my first
    priority.  Sorry for this annoyance.
