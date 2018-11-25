Diffstat is a simple implementation of diffstat unix tool in Emacs.  This
mode can be used as standalone for a certain diff file but it will be more
useful with vc mode.

You can simply do M-x diffstat in diff-mode buffer to get diffstat.

C-q / q      : Quit
C-c / C-q    : Go back to the place in the diff file corresponding
             : to file on the current line


You might want to add the following in your .emacs:

(require 'diffstat)
(add-hook 'diff-mode-hook (lambda () (local-set-key "\C-c\C-l" 'diffstat)))


NOTE : It only works with unified-diff format.
