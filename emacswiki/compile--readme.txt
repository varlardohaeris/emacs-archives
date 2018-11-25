   Extensions to `compile.el'.

 See also the companion file `compile+.el', if you are using Emacs
 22 or later.

       `compile-.el' should be loaded before `compile.el'.
       `compile+.el' should be loaded after `compile.el'.

 Put this in your initialization file (`~/.emacs'):

   (require 'compile-)

 Face suggestion (what I use):

   `next-error': SkyBlue background, no inheritance


 New face defined here:

 `compilation-mouseover' - Use instead of highlight for mouse-face.

 Function `fit-1-window-frames-on' (defined in `fit-frame.el') is
 added here to `compilation-finish-functions'.


 ***** NOTE: The following variable defined in `compile.el'
             has been REDEFINED HERE:

 `compilation-message-face' -
    We set the default value to nil, to get rid of underlining.
