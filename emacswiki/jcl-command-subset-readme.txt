When hitting M-x there is a bazillion of commands to choose from.
With jcl-define-command-subset one can create a command that works
like M-x but the completion only consider a subset of available
commands.  Which commands are part of that subset is determined by
a regex passed to jcl-define-command-subset.

Example usage:

Define a subset with all commands that starts with "jcl-".
(jcl-define-command-subset jcl-command-subset "M-z " "^jcl-" "jcl-")

Define a subset with all commands which have the string "toggle"
in their name.
(jcl-define-command-subset jcl-toggle-command-subset "M-Z " "toggle")

Define a subset with all monkey related commands.
(jcl-define-command-subset jcl-monkey-command-subset
                           "Select monkey business: "
                           "monkey\\|ape\\|chimp\\|gorilla")

(define-key global-map [(meta ?z)] 'jcl-command-subset)
(define-key global-map [(meta ?Z)] 'jcl-toggle-command-subset)
(defalias 'monkey 'jcl-monkey-command-subset)

(jcl-command-subset-setup)


Note:
Implementation of this library could be simplified by something
like:
(let ((completion-regexp-list '("monkey")))
  (read-command "Cmd: "))
But ido do not seem to care about completion-regexp-list.
