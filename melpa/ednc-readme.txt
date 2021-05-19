The Emacs Desktop Notification Center (EDNC) is an Emacs package
written in pure Lisp that implements a Desktop Notifications service
according to the freedesktop.org specification.  EDNC aspires to be
a small, but flexible drop-in replacement of standalone daemons like
Dunst.  A global minor mode `ednc-mode' tracks active notifications,
which users can access by calling the function `ednc-notifications'.
They are also free to add their own functions to the (abnormal) hook
`ednc-notification-amendment-functions' to amend arbitrary data and
to the (abnormal) hook `ednc-notification-presentation-functions' to
present notifications as they see fit.  To be useful out of the box,
default hooks record all notifications in an interactive log buffer
`*ednc-log*'.
