Insert template into buffer, performing tag expansions.  Inspired by
auto-insert and auto-insert-tkld.

See `file-template-tag-alist' for predefined tags.

Add this to your .emacs if you want to insert templates explicitly:

(autoload 'file-template-auto-insert "file-template" nil t)

You can also have templates inserted into new files automatically
by setting `file-template-insert-automatically' appropriately and
adding this to your .emacs:

(autoload 'file-template-find-file-not-found-hook "file-template" nil t)
(add-hook 'find-file-not-found-hooks 'file-template-find-file-not-found-hook 'append)
