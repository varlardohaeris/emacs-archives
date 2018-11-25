INSTALLATION: In your ~/.emacs,

(require 'vc-jump)
(global-set-key [f12] 'vc-jump)   ; for your convenience



`vc-jump' switches the current buffer (possibly in another window)
to the Version Controlled status buffer. (inspired by `dired-jump')

For example, if you're editing a file controlled by CVS, `vc-jump'
executes `cvs-status' in the file's directory.  Likewise, if the
current file is controlled by GIT, `vc-jump' executes `git-status'
in the file's directory.
