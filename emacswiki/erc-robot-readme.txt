This code implements a simple robot for ERC.

Installation:

The robot uses hooks to gain access to ERC.  The following need to
be executed after ERC has loaded:

    (load-library "erc-robot")
    (add-hook 'erc-server-PRIVMSG-functions 'erc-robot-remote t)
    (add-hook 'erc-send-completed-hook 'erc-robot-local t)

It is particularly important that the remote robot function is added
to the tail of the PRIVMSG hook.

Robot commands are declared using the list "erc-robot-commands".
TODO: better description of the functions.
An example might be:

(setq erc-robot-commands
      '(
	("cmds" t (lambda (args)
		  (concat "commands available: "
			  (mapconcat
			   (lambda (e)
			     (car e))
			   erc-robot-commands " "))))
	("hello" t (lambda (args) "hello to you too !"))
	("zippy" t (lambda (args) (erc-replace-regexp-in-string "\n" " " (yow))))
	("echo" t (lambda (args) args))
	; only i'm allowed to talk to my doctor !
	("version" t (lambda (args) (erc-version)))
      ))
