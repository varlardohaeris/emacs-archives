   Extensions to `frame.el'.


 ***** NOTE: The following function defined in `frame.el' has been
             REDEFINED HERE:

 `special-display-popup-frame' - Call `fit-frame'.

 NOTE: Starting with Emacs 24, `special-display-popup-frame' was
       moved to `window.el' from `frame.el'.  I have therefore
       moved my enhancement of it from `frame+.el' to my library
       `window+.el'.  This means that `frame+.el' is now OBSOLETE.
       I leave it posted in case someone with an older release does
       not want the additional enhancements that are included in
       `window+.el'.

 This file should be loaded after loading the standard GNU file
 `frame.el'.  So, in your `~/.emacs' file, do this:
 (eval-after-load "frame" '(require 'frame+))
