Check the size of all files in a repertory
and warn if a file is bigger than 0
launch tv-launch-mail-system if you want to start fetchmail
and then warn-mail.
if your system is always running fetchmail from boot
run only tv-launch-warn-without-fetch
Quick start:(set to your convenience)

Copy the file somewhere in your path
add these lines to your .emacs

(require 'warn-mail)
(tv-launch-warn-without-fetch)

set mail-list-to-watch in customize or ex:
(setq mail-list-to-watch (list
			     "/home/you/incoming/default"
                          "/home/you/incoming/friends"))

If you want to be notified in the mode-line, you have to set in .emacs or customize:
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)
(setq message-mode-line t)

If you want an icon to show when there is no mails add
(setq display-time-use-mail-icon t)

keybindings you can use:

(global-set-key (kbd "C-c f m") 'tv-launch-mail-system)
(global-set-key (kbd "C-c q f m") 'tv-stop-warn-and-fetch)

You will find all the config in customize==>applications==>mail==>|display-time
                                                                  |warn-mail
