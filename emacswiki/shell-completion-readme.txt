Put this file into your load-path and the following into your ~/.emacs:
(require 'shell-completion)

If you want use with lftp, put this to .emacs
(defvar my-lftp-sites (shell-completion-get-file-column "~/.lftp/bookmarks" 0 "[ \t]+"))
(add-to-list 'shell-completion-options-alist
             '("lftp" my-lftp-sites))
(add-to-list 'shell-completion-prog-cmdopt-alist
             '("lftp" ("help" "open" "get" "mirror") ("open" my-lftp-sites)))
