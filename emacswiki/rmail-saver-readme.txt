This package does 2 things. It:

1) parses your current message for a From: or Sender: line and
intelligently creates a file name according to your specifications,
i.e. it will save mail from andy@some.regular.path to a file
called andy or a file called andy@some or a file called
andy@some.regular or a file called andy@some.regular.path in a
directory  that you specify.

It will save in either rmail or regular mail (inbox) format.

The address parser also understands uucp style bang paths and
converts them to internet style paths when creating the file name.

2) Includes functions to do this for all the undeleted messages in your
RMAIL file and then will mark them for deletion.

To save the current message in a file, use
M-x daves-rmail-save-this-message-babyl
or
M-x daves-rmail-save-this-message-inbox

which parses the message, then saves into either babyl (RMAIL) or
inbox (regular mail) files in the directory saved-rmail-file-directory.

To save all files that are currently not marked for deletion, use
M-x daves-rmail-save-messages-babyl
or
M-x daves-rmail-save-messages-inbox

The files are saved under the directory saved-rmail-file-directory.
the default for this is ~/RMail/

Now some more details to allow you to screw around with the
filename creation scheme. There are 3 user-defined variables that
control the 3 levels of filename length and content checking. They are:
user-machine-domain-and-company-length (default value 15)
user-machine-and-domain-length (default value 12)
user-and-machine-type-address-length (default value 7)

First of all, by making any of these variables nil, only the user
name (the first part of the address) is returned as the file name.

Each is the maximum length of an incoming address part - These
variables define where to truncate the mail address when making a
file name. For addresses of the type "user@machine.domain.company"
or longer, only the first 3 names are looked at (up to the company
field). I'm no RFC 822 pro so I just called them user, machine,
domain, and company. If someone's got good info on this, I'd be
glad to change the names to the correct ones.

user-machine-domain-and-company-length deals with addresses of the
type "user@machine.domain.company". If the address is shorter than
this length, then that address is returned as the file name.
Otherwise, the other two levels of checking are done. To
disable this level of checking but keep the other two, set
user-machine-domain-and-company-length to 0.

user-machine-and-domain-length deals with addresses of the
type "user@machine.domain". If the address is shorter than this
length, then the address is returned as the file name. Otherwise
the last level of checking is done. To disable this level of
checking but to keep the other two, set
user-machine-and-domain-length to 0.

user-and-machine-type-address-length deals with addresses of the
type "user@machine". If the address is shorter than this length,
then the address is returned as the file name. If the length of
the address is greater than user-and-machine-type-address-length
then the default file name, the user name is returned.

If the mail is local (of the type "user") then it is made the file
name.

If the variable daves-rmail-prompt-for-filename is non-nil, the
functions in rmail-saver will give you a prompt allowing you to
enter your own filename when saving, and the parsed filename
becomes the default. If nil, rmail-saver works quietly, using only
the parsed filenames when saving.


There are 2 hooks: daves-rmail-before-save-message-hook and
daves-rmail-after-save-message-hook which are called before and
after messages are saved, respectively. These hooks are only run
for daves-rmail-save-messages-babyl and
daves-rmail-save-messages-inbox calls.

Since this package makes use of mail-extract-address-components from
mail-extr.el, that package is needed. It is packaged as part of
the 19.19 distribution.

I'm always looking for improvements, suggestions, bug reports,
criticisms, etc of the code. If you have a patch, please send it
to me and I'll incorporate it into the next version. This is my
first major piece of lisp code that I'm putting out to the net, so
be warned, and be gentle. I consider this beta code - It works on
my setup of emacs, but there may be problems on other setups.


To Do: Set it up to save only messages marked with a certain tag
       or via a regexp. Clean up the code, especially the parser.
       Set up the Classify menu so that the original menu options
       are at the top, and the new options are on the bottom.
       Maybe someone with lots of keymap experience can fix this?

Bugs: It has some problems with mail headers using % - my bug fix
just changes all of the % in the headers to . after the parsing is
done. This does not affect the parsing routine. Anyone know of a
better way to do this to maintain the % ? Emacs thinks that it is
part of a format command.


To use:

You'll need to either create a directory called RMail in your home
directory (note the capitalizations) or change the variable
saved-rmail-file-directory to the directory you want messages
saved to.

Add this to your .emacs file:
(autoload 'daves-rmail-save-messages-babyl "rmail-saver" nil t)
(autoload 'daves-rmail-save-messages-inbox "rmail-saver" nil t)
(autoload 'daves-rmail-save-this-message-babyl "rmail-saver" nil t)
(autoload 'daves-rmail-save-this-message-inbox "rmail-saver" nil t)

If you want rmail-saver to prompt you each time it saves a message, put
this in your .emacs:
(setq daves-rmail-prompt-for-filename t)

If you want to bind these functions to key mappings and menu bar items in
RMAIL and RMAIL-summary modes, I use:

(setq rmail-mode-hook
      '(lambda ()
  (define-key rmail-mode-map "\C-co"
    'daves-rmail-save-this-message-babyl)
  (define-key rmail-mode-map "\C-c\C-o"
    'daves-rmail-save-this-message-inbox)
  (define-key rmail-mode-map "\C-c\eo"
    'daves-rmail-save-messages-babyl)
  (define-key rmail-mode-map "\C-c\e\C-o"
    'daves-rmail-save-messages-inbox)
  (define-key rmail-mode-map [menu-bar classify dave-clear-to-inbox]
    '("Clear box (inbox)" . daves-rmail-save-messages-inbox))
  (define-key rmail-mode-map [menu-bar classify dave-clear-to-rmail]
    '("Clear box (Rmail)" . daves-rmail-save-messages-babyl))
  (define-key rmail-mode-map [menu-bar classify dave-output]
    '("Save msg (inbox)" . daves-rmail-save-this-message-inbox))
  (define-key rmail-mode-map [menu-bar classify dave-output-inbox]
    '("Save msg (Rmail)" . daves-rmail-save-this-message-babyl))
  ))
(setq rmail-summary-mode-hook
      '(lambda ()
  (define-key rmail-summary-mode-map "\C-co"
    'daves-rmail-save-this-message-babyl)
  (define-key rmail-summary-mode-map "\C-c\C-o"
    'daves-rmail-save-this-message-inbox)
  (define-key rmail-summary-mode-map "\C-c\eo"
    'daves-rmail-save-messages-babyl)
  (define-key rmail-summary-mode-map "\C-c\e\C-o"
    'daves-rmail-save-messages-inbox)
  (define-key rmail-summary-mode-map
    [menu-bar classify dave-clear-to-inbox]
    '("Clear box (inbox)" . daves-rmail-save-messages-inbox))
  (define-key rmail-summary-mode-map
    [menu-bar classify dave-clear-to-rmail]
    '("Clear box (Rmail)" . daves-rmail-save-messages-babyl))
  (define-key rmail-summary-mode-map
    [menu-bar classify dave-output]
    '("Save msg (inbox)" . daves-rmail-save-this-message-inbox))
  (define-key rmail-summary-mode-map
    [menu-bar classify dave-output-inbox]
    '("Save msg (Rmail)" . daves-rmail-save-this-message-babyl))
     ))
which binds C-c o to daves-rmail-save-this-message-babyl
            C-c C-o to daves-rmail-save-this-message-inbox
            C-c M-o to daves-rmail-save-messages-babyl
            C-c M-C-o to daves-rmail-save-messages-inbox

in the RMAIL and RMAIL-summary buffers

and which also sets up 4 new menu options in the Classify menu of
rmail and rmail-summary modes. Anybody know of a good way to have
these options put at the end of the menu?
