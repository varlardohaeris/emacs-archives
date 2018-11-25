 2002-12-28 checked and edited for bbdb CVS version + mew 3.0.51 + emacs 21.1

Installation:

Put bbdb-mew.el in your load path, so that emacs can find it.

Run a patched bbdb-com.el to allow mew to be the mailer bbdb
uses if bbdb-send-mail-style is set to 'mew

Insert the following lines in your ~/.emacs:
     (other BBDB stuff comes here)
             :
(autoload 'bbdb-insinuate-mew      "bbdb-mew"   "Hook BBDB into Mew")
(add-hook 'mew-init-hook 'bbdb-insinuate-mew)
(setq bbdb-send-mail-style 'mew)

To use BBDB name at From: field of header in citation, please set
(setq mew-cite-bbdb-header t)
