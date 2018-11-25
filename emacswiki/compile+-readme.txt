   Extensions to `compile.el'.

 See also the companion file `compile-.el'.
       `compile-.el' should be loaded before `compile.el'.
       `compile+.el' should be loaded after `compile.el'.

 Put this in your initialization file (`~/.emacs'):

   (require 'compile+)

 Additional keys are bound here.  Some bindings that would normally
 try to modify a compilation mode buffer are unbound, so they are
 available for local (Compilation Mode) definition.


 ***** NOTE: The following variable defined in `compile.el'
             has been REDEFINED HERE:

 `compilation-error-regexp-alist-alist' -
    Regexp matches whole line, so mouse-over it.


 ***** NOTE: The following macro defined in `compile.el'
             has been REDEFINED HERE:

 `compilation-assq'.


 ***** NOTE: The following functions defined in `compile.el'
             have been REDEFINED HERE:

 `compilation-compat-error-properties',
 `compilation-directory-properties', `compilation-goto-locus',
 `compilation-internal-error-properties'.
