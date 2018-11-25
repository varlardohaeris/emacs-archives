Insert pair of parenthesis.
If region is active and transient-mark-mode is not nil, insert
enclosing strings at region boundaries.
If the letter before the point is a backslash this packages's
functions do not work.
(This behavior is to make it easy that you write regular expression.)
In that case, packages functions work as the normal self-insert-command.

Functions:

style of {}   : parenthesis-insert-braces
style of []   : parenthesis-insert-brackets
style of ()   : parenthesis-insert-parens
style of ''   : parenthesis-insert-single-quotation
style of ""   : parenthesis-insert-double-quotation
style of <>   : parenthesis-insert-angle

style of `'   : parenthesis-insert-grave-and-quotation

style of {  } : parenthesis-insert-braces2
style of [  ] : parenthesis-insert-brackets2
style of (  ) : parenthesis-insert-parens2
style of '  ' : parenthesis-insert-single-quotation2
style of "  " : parenthesis-insert-double-quotation2
style of <  > : parenthesis-insert-angle2

Variables:

parenthesis-func-alist :

  list of functions.

parenthesis-insert-ignore-backslash :

  If non-nil, insert pair of parensis even a backslash
  exists bofore point.

parenthesis-push-mark :

  Non-nil means to push mark to end point of close paren
  when region has been selected.
  Default value is t.

Prefix Arg Example:

C- - C-4 M-x parenthesis-insert-parens => ((((
C-0      M-x parenthesis-insert-parens => (
C-4      M-x parenthesis-insert-parens => (((())))

Example of Setting:

This package provide functions only.
So, it is necessary to do key setting by yourself.
If you do key setting as a whole, you can use a
parenthesis-register-keys function.
Usage example of this function is as follows.

(add-hook
 'scheme-mode-hook
 (lambda()
   (parenthesis-register-keys "(\"[" scheme-mode-map)))

This do key settings for scheme-mode.
In this, it insert a group of (, " and [ in a mass.
And, if c-mode, it will be as follows.

(add-hook
 'c-mode-hook
 (lambda()
   (parenthesis-register-keys "{('\"[" c-mode-map)))

In this, it insert a group of {, (, ', " and [ in a mass.

And, if you want to insert a <div></div> in a mass,
(and the point is just after <div>.)
add the following lines in your ~/.emacs.

(add-to-list 'parenthesis-func-alist
             '(parenthesis-insert-divs "<div>" "</div>"))
(parenthesis-init)

The point position after inserting can be set.
The following setting is the point is not after the linefeed.
The point is after the '>'.

(add-to-list 'parenthesis-func-alist
             '(parenthesis-insert-divs "<div>\n" "</div>" 5))
(parenthesis-init)

Difference of insert-pair:

Main Difference is follows.

  - insert-pair is char.
  - parenthesis is string.
  - 1st argument of insert-pair enclose following arg sexps in
    a pair of open and close characters.
  - 1st argument of parenthesis repeats arg time.
  - insert-pair is Emacs built-in function.
  - parenthesis is external elisp package.
