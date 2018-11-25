   Extensions to `outline.el'.

 Menu bar:

 Reordered and renamed menu bar Outline Mode Hide and Show menus.
 Names of menu items now indicate whether the item applies locally
 or globally.  Global and local items are separated in the menu.

     Renamings:
       Hide Leaves    -> Hide Entries                (local)
       Hide Body      -> Hide All But Headings       (global)
       Hide Subtree   -> Hide Tree                   (local)
       Hide Sublevels -> Hide All But Top N Headings (global)
       Hide Other     -> Hide All But Entry          (local)
       Show Branches  -> Show Headings               (local)
       Show Children  -> Show Headings N Deep        (local)
       Show Subtree   -> Show Tree                   (local)

 Outline minor mode font locking:

    See the new command `toggle-outline-minor-mode-font-lock',
    intended for use as both `outline-minor-mode-hook' and
    `outline-minor-mode-exit-hook'.


 New function defined here: `toggle-outline-minor-mode-font-lock'.

 New variables defined here:

    Var `outline-minor-mode-hook' was not declared in `outline.el'.
    It is declared here, along with `outline-minor-mode-exit-hook'.


 ***** NOTE: The following function defined in `vc.el' has been
             REDEFINED HERE:

 `outline-minor-mode' - Call to `outline-minor-mode-exit-hook'.


 This file should be loaded after loading the standard GNU file
 `outline.el'.  So, in your `~/.emacs' file, do this:
 (eval-after-load "outline" '(progn (require 'outline+))
