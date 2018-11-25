   Extensions to `diff-mode.el'.

 "*Diff*" buffer is highlighted differently.

 NOTE: The faces defined here look best on a medium-dark
       background, because some are light and some are dark.
       Try, for example, setting the background to "LightSteelBlue"
       in your `~/.emacs' file: You can do this is via
       `special-display-buffer-names':

        (setq special-display-buffer-names
              (cons '("*Diff*" (background-color . "LightSteelBlue"))
                    special-display-buffer-names))

       You can alternatively change the background value of
       `special-display-frame-alist' and set
       `special-display-regexps' to something matching "*info*":

        (setq special-display-frame-alist
              (cons '(background-color . "LightSteelBlue")
                    special-display-frame-alist))
        (setq special-display-regexps '("[ ]?[*][^*]+[*]"))


 New faces defined here:

   `diff-file1-hunk-header', `diff-file2-hunk-header'.


 ***** NOTE: The following faces defined in `diff-mode.el' have
             been REDEFINED HERE:

   `diff-added', `diff-changed', `diff-context',
   `diff-file-header', `diff-header', `diff-hunk-header',
   `diff-index', `diff-indicator-added', `diff-indicator-changed',
   `diff-indicator-removed', `diff-nonexistent', `diff-removed'.


 ***** NOTE: The following variables defined in `diff-mode.el' have
             been REDEFINED HERE:

   `diff-context-mid-hunk-header-re', `diff-font-lock-keywords',
   `diff-hunk-header-re-unified'.


 This library should be loaded *before* library `diff-mode.el'.
 Put this in your initialization file, `~/.emacs':
   (require 'diff-mode-)
