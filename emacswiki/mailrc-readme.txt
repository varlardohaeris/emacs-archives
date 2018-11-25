Installation:
-------------
Mailrc-mode is intended to be used in conjunction with
message-mode for composing messages, I therefor suggest
the following installation procedure:

1. Byte compile mailrc.el with M-x byte-compile-file, and
   move the resulting mailrc.elc to the same directory as
   you have message.elc.

   You can of course put the mailrc.elc anywhere in your
   emacs load path. If you want to add a directory to your
   emacs load path you can do that with the following in your
   .emacs file:

	    (setq load-path (cons "~/personal-lisp" load-path))

   This statement will add the directory ~/personal-lisp to
   (the front of) your load-path list.


2. Add the following autoload statement to your .gnus or
   .emacs file:

	 (autoload 'mailrc-mode "mailrc" "Load the file \"mailrc.el(c)\" when the command mailrc-mode is invoked." t)


3. Finally I suggest to add a keybinding like the following,
   so that mailrc-mode can be quickly invoked from message-mode:

   (add-hook 'gnus-started-hook '(lambda ()
   		(define-key message-mode-map "\C-ca" 'mailrc-mode)))


Then you should be ready to test it, type M-x mailrc-mode from
any mode, and hopefully mailrc-mode starts up. Finally you can go
to message-mode to compose a message and try the keybinding defined
under 3. above.

I hope it works for you - "Joakim Hove <hove@phys.ntnu.no>"
