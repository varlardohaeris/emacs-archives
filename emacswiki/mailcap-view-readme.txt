mailcap-view.el is a set of functions that allows you to do general
file browsing and viewing from within emacs.  The pimary function
is mailcap-view-file which launches a shell-command using the
default mime-handler.  The find-file-hooks function supplied allows
you to open files who's mime-type is text/* or nil in the normal
way.  However, files whos mime-type is defined and not text/* are
launched with an external viewer.  In short, hitting return in a
dired buffer on a .jpg will display the jpeg if you have your
mailcap setup properly.

I'm sure the style of this code isn't optimal.  Feel free to hack
on this and send me patches.
