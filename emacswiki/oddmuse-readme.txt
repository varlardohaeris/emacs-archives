A simple mode to edit pages on Oddmuse wikis using Emacs.

Since text formatting rules depend on the wiki you're writing for,
the font-locking can only be an approximation.

Put this file in a directory on your `load-path' and
add this to your init file:
(require 'oddmuse)
(setq url-proxy-services '(("http" . "your.proxy.host:portnumber")) ; if needed
(oddmuse-mode-initialize)
And then use M-x oddmuse-edit to start editing.
