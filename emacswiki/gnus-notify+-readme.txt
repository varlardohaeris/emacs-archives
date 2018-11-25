Gnus notify


Installation:

Put gnus-notify+.el to your load-path.
The load-path is usually ~/elisp/.
It's set in your ~/.emacs like this:
(add-to-list 'load-path (expand-file-name "~/elisp"))

And the following to your ~/.emacs startup file.

(require 'gnus-notify+)

I recommended add `gnus-notify+' with below hooks:

(add-hook 'gnus-summary-exit-hook 'gnus-notify+)
(add-hook 'gnus-group-catchup-group-hook 'gnus-notify+)
(add-hook 'mail-notify-pre-hook 'gnus-notify+)
