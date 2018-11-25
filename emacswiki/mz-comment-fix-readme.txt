Description:

This file overwrites several of emacs' functions to comment and uncomment
regions. A bug is present in the original versions which prevents nested
comments from working correctly in all languages. For example, in XML and
related languages, commenting out this code:

<!-- comment -->

Produces this:

<!-- <\!-- comment -\-> -->

Unfortunately, this is not valid XML: comments may not contain the '--'
substring.

This file corrects this oversight by adding two variables in which modes can
give Emacs information about how they would like their nested comments
quoted. For the above example, with the correct configuration (see below), a
nested comment looks like this:

<!-- <!-\- comment -\-> -->

Which *is* valid XML. Note of warning: this code is currently almost entirely
untested. Emacs' original behaviour can be restored by executing
`comment-fix-disable-mz'.

Installation:

o Place this file in a directory in your load-path.

o Put the following in your .emacs file:
    (require 'mz-comment-fix)

o For each mode that currently has incorrect nested quoting behaviour, set
  the correct point at which to break. Two alists exist, one for the start of
  comments, one for the end. For example, for xml-mode, the correct
  configuration is:

    (add-to-list 'comment-strip-start-length (cons 'xml-mode 3))

o Modes that already have the correct behaviour need not be added. Restart
  your Emacs or reload your .emacs to apply the changes.

o mz-comment-fix.el is now installed. Whenever you comment or uncomment a
  nested comment, the new code will correctly mangle or unmangle the nested
  comment.
