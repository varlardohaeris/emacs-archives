Work together a GoogleChrome extension "Edit with Emacs"
and elscreen.el.

1. Download and put this file to your `load-path' directory.
2. Require `elscreen.el', `edit-server.el' and this file.

For example:

(require 'elscreen)
(require 'edit-server)
(require 'elscreen-edit-server)


This file `defadvice' to function of edit-server.el.

- edit-server-done
- edit-server-create-frame
