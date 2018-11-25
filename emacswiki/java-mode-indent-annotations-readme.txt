This library tries to indent java annotations
(http://java.sun.com/j2se/1.5.0/docs/guide/language/annotations.html)
like the code examples listed in the webpage.

To use this library first ensure that
java-mode-indent-annotations.el is in your load path and load it
like so :
(require 'java-mode-indent-annotations)

Next, call the `java-mode-indent-annotations-setup' function to your
`java-mode-hook' or `jde-mode-hook' after any calls to
`c-set-style'.

The setup function adds one of the custom indentation functions
`c-single-indent-after-java-annotations' or
`c-no-indent-after-java-annotations' to the offset lists of the
symbols arglist-intro, topmost-intro-cont, arglist-intro,
arglist-close, statement-cont and func-decl-cont.  See the
documentation strings for `c-single-indent-after-java-annotations'
and `c-no-indent-after-java-annotations' for more info on what they
do.
