This package provides syntax highlighting for the Sudo security
policy file, /etc/sudoers.

If Flycheck is present, it also defines a Flycheck syntax checker
using visudo.

Please don't edit /etc/sudoers directly.  It is easy to make a
mistake and lock yourself out of root access.  Instead, don't be put
off by the name: use visudo.  You can do that by setting up
emacsclient, or by using the function (etc-sudoers-mode-visudo).
