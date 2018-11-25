-------------
Background:
-------------

Sometimes I use multi windows to do my job, and when i
switch to other buffer and go back, the original layout is
gone.  I have to set it up again.
I was very annoyed about this so I tried other packages to
help me out.  I found `WinnerMode', which always did
something I didn't want, and `TaskMode', which seems too
powerful to be used simply.  So I wrote this by myself.
Actually this package is my first emacs extention package
and I don't really know if I have made it well.  But I think
this package is already quite useable.



-------------
Commands:
-------------

`layout-save-current' save the current window-configuration
as layout so that when next time you switch back to this
buffer, the layout will be brought back automatically.  You
can also manually use `layout-restore' to restore the layout.
When you feel a layout is no more needed, switch to the
buffer where saved it and use `layout-delete-current' to
delete this layout.  These codes are simple and well
documented, you can easily hack it by yourself.



-------------
Set it up:
-------------

To start using this package, add following lines to your emacs
startup file.
---------------------------------------------------------------
(require 'layout-restore)
save layout key
(global-set-key [?\C-c ?l] 'layout-save-current)
load layout key
(global-set-key [?\C-c ?\C-l ?\C-l] 'layout-restore)
cancel(delete) layout key
(global-set-key [?\C-c ?\C-l ?\C-c] 'layout-delete-current)
---------------------------------------------------------------
Change the keybindings to whatever you like.



-----------------
Detailed example:
-----------------

Let's suppose you are working in buffer A and the emacs now
looks like:

+-------+
|   A_  |
+---+---+  (layout 1)
| B | C |
+---+---+

Now for some reason, you want to switch to other buffer and
do some other thing, these make your emacs looks like:

+---+---+
|   |   |
| D_| E |  (layout 2)
|   |   |
+---+---+

Now you are working in buffer D and want to switch back to
buffer A.  Usually you can do this simply by `C-x b RET'.
But what about buffer B and buffer C?  They won't be back
automatically when you switch to buffer A.  So your emacs
looks like below:

+---+---+
|   |   |
| A_| E |
|   |   |
+---+---+

Well, I am sure this is NOT what you want.

By using this package, when you are in buffer A of layout 1,
press `C-c l' to remember this layout. Then switch to buffer
D to your work.  Now when you switch back to buffer A, the
buffer B and C will be brought back automatically, and be
placed exactly as where they were.

If you want, you can alsa remember layout 2 when you are in
buffer D.  simply press `C-c l' to remember it, then you can
switch between layout 1 and layout 2 easily when you switch
between buffer A and D.

To unmemorise a layout, simply press `C-c C-l C-c' in the
buffer where you press `C-c l' before.



-----------
Contact me:
-----------

Any question, or advice, please mail to

XYZvektorXYZ@XYZyeahXYZ.net

remove all XYZ from above address to get the real one.
