Integrate auto-install.el with anything.el.

You can use command `anything-auto-install-from-emacswiki'
install package from EmacsWiki.org.

You can use comamnd `anything-auto-install-from-library'
update library.

You can also make this package integrate with `anything',
just setup like below:

(setq anythign-sources
      (list
       anything-c-source-auto-install-from-emacswiki
       anything-c-source-auto-install-from-library
       ))
