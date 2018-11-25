The overall idea behind this is to provide completion
functionality when using plt scheme.
The initial plan is similar to what help desk does at the
moment and load the doc keywords into memory, and then use them to
help with completion.

In the future in an ideal world I would like to either hook
mzscheme into slime or use a similar technique to hook into a
running mzscheme process allowing interrogation of active objects.
One thing I have noticed with slime is that it tended to bork when
running in vmware, which is not very handy considering how much
development can happen using these tools.  Just possibly this is
related to the use of sockets.

This document contains two different pieces of functionality, the
mode for displaying the completions and the code to actually
calculate the completions.

USAGE:

NOTE: This package uses the common lisp package, which creates a
certain amount of namespace pollution.  If you are not comfortable
with this, I would suggest using the fabulous help-desk provided by
the plt scheme distribution instead.  Being a common lisp slut, I
have this set on by default anyway.

The pivotal function for this library is plt-completion-command so
all you need to do is bind this to a key you would find convenient
when you want completion.  Here is what sits in my .emacs
(require 'plt-completions)
(define-key scheme-mode-map [(f3)] 'plt-completion-command)
(define-key scheme-mode-map (kbd "TAB") (indent-or-complete plt-completion-command))

If you would prefer that the completions window if it needs to be
created should split vertically (one above the other) add this:
(setq *plt-buffer-split-horizontally* nil)

If you would like to only match from the beginning of a symbol
instead of any match add this:
(setq *plt-match-all* nil)

The code from the last binding is here:
(defmacro indent-or-complete (&rest complete)
  "A useful pattern for binding the tab (or other) key to, but needs
to be able to take specific function names and calls."
  `(lambda ()
     "Complete if point is at the end of a word, otherwise indent line."
     (interactive)
    (if (looking-at "\\_>")
     (cond ((thing-at-point 'symbol)
            (,@complete))
           ((thing-at-point 'whitespace)
            (indent-for-tab-command)))))

NOTE: This sometimes threw an error, but has decided to stop, so no
guarantees, but it is very useful, if it behaving well.

LIMITATIONS

As this was developed to serve a very specific purpose and is very
much alpha software, I have only tested it on the platform I
developed it on: Emacs 22 (cvs & multi-tty).  It should work with
Emacs 21 as there is nothing cutting edge in the code but I have
not tested it. As for xemacs, I don't use it so assume that this
code only works by coincidence, but I am happy to apply any patches
to make it work with xemacs as long as it does not break anything
in emacs.

FEATURES
I am not sure if you are supposed to list features but to make sure
there are no surprises for someone who actually has the patience to
read through all the preamble.  Not very much to it really, but I
have found it very handy, when developing.

* As soon as you hit your bound key in a relevant buffer (read
  scheme) it will look for your plt docs directory, and if it can't
  find one will ask you to locate it for it, and then go about its
  business.

* When creating a completion it will split your buffer according to
  the setting of *plt-buffer-split-horizontally*.  If it is set to
  true (the default) it will split the frame horizontally (split
  into two lengthwise chunks, I can never get the terminology
  sorted out), and display the possible completions in the other
  buffer.  If the frame is already split it will just reuse an open
  window. To have the window split vertically, just set this
  variable to nil.  (Thanks to jao (Jose A. Ortega Ruiz) for
  feedback on this)

* When the display is showing, hitting the completion command again
  will check if there is a change to the search pattern, and if not
  will just scroll the completion buffer.  If you reach the end of
  the buffer, on the next completion request it will cycle to the
  top of the buffer and start down again.

* When the cursor is in a plt completions buffer, there are two key
  combinations that actually do something.

  * C-return when sitting on a line with a completion will attempt
    to find the documentation (using browse-url-at-point) for that
    item, using the local documentation.  Note: sometimes the decs
    will not be there, because only the keyword file has been
    installed.  To make this more useful install the rest of the
    docs.  A todo is to make this more robust, so if it cannot find
    the reference it looks on line.
  * Return - This replaces the symbol at point in the buffer that
    called for the completion with the completion where point is.
    This is not a pretty system at the moment, and improving it is
    on the TODO list.

At the moment (since I have only just released it, this is a one
person effort.  I hope you find it useful, and please let me know
if anything is not working for you, and I will see what I can do to
fix it (time permitting).
