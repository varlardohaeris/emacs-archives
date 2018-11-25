This is the Emacs interface to MoiKrug (http://moikrug.ru) service.
It has only two commands:

  moikrug-bbdb-invite   - Invite person from bbdb to your MoiKrug.
  moikrug-bbdb-merge-in - Merge MoiKrug entries into bbdb.

moikrug.el depends on w3, net-utils, gnus and bbdb packages, make
sure they are installed.

Sample configuration might look like:

  (autoload 'moikrug-bbdb-invite "moikrug" nil t)
  (autoload 'moikrug-bbdb-merge-in "moikrug" nil t)

  (setq moikrug-login user-mail-address)

  (add-hook 'bbdb-mode-hook
            (lambda ()
              (define-key bbdb-mode-map [?u] 'moikrug-bbdb-merge-in)
              (define-key bbdb-mode-map [?i] 'moikrug-bbdb-invite)))
