I work with files. Lots of files. So I want to open files as
effortlessly as possible. This package is a part of the quest for the
effortless file opening.

I noticed lately that I use the file history more and more for opening
files. I use savehist with a history length of 1000, duplicates
filtered out, so there is a good chance a file I want to open is in
the history.

I use Icicles, so I can use it to match files from the history:

  find-file <delete input> <type pattern> M-h

Pretty easy. But not effortless enough for a frequent operation. In my
opinion at least.

Why do I have to tell emacs I want to complete a file from the
history, not from the file system? It should simply tell me if there
is a match from the history without me doing anything explicitly.

And that's what timid completion does.

When find-file is started there is nothing unusual. If the user
presses TAB, or looks for a file with UP/DOWN in the history, or
anything then everything works as expected.

If the user simply types a few characters and hesitates a bit
(configurable) then timid completion looks for matches in the history
and displays them if there is any and the user can select a match with
the UP/DOWN keys and ENTER (so these keys are redefined if there are
matches to select from). If more characters are typed the list of
matches is updated. If any other command (e.g. TAB) is used then timid
completion disables itself.

So timid completion shows matches from the history automatically while
being as transparent as possible. It appears only if it can offer
something to choose from and disappears instantly if the user chooses
to ignore it. It's timid, you know.

Timid completion can be enabled with

	(timid-mode t)

This enables timid completion mode, but you also have to specify
which commands you want to have affected by timid.

Timid completion can be turned on for all commands by customizing
variable `timid-enable-globally'. Alternatively, it can be enabled
for individual commands by putting property `timid-completion' with
value `enabled' on the command. Also, it can be enabled globally
and be disabled for individual commands by setting value of
property `timid-completion' to `disabled' for the command. See
below for a complete list of possible command properties.

Iswitchb requires special treatment, so use this command to enable
timid for it:

	(timid-iswitchb-setup)


In order to take full advantage of timid completion use of package
savehist is recommended with a sufficently high history-length
value (I use 1000).

Also it is recommended to filter duplicates from the file history
and push every file opened or written in Emacs on it, so they can
be reopened easily if they are needed again:

     (defun my-add-file-hook ()
      "Add the name of the file just opened or written to
     `file-name-history'"
      (and buffer-file-name
           (progn (setq file-name-history
                        (delete buffer-file-name file-name-history))
                  (push buffer-file-name file-name-history)))
      nil)

     (add-hook 'find-file-hooks  'my-add-file-hook)
     (add-hook 'write-file-hooks 'my-add-file-hook)

I use Emacs 21, there may be a builtin method for the code above
in Emacs 22.


Complete list of command properties for timid completion:

  Property: timid-completion

  Values:

    enabled - Timid is enabled for commands having this
              property. This is not necessary if you enabled timid
              globally via variable `timid-enable-globally'.

    disabled - Timid is disabled for this command. This is useful
               if you have enabled timid globally via variable
               `timid-enable-globally', but want to inhibit it for
               certain commands.

    allowed - The command is allowed to use during timid completion
              without terminating it.

    edit - The command is used to modify the search pattern during
           timid completion.


  Property: timid-search-delay-function

  Value: Function to determine how much should timit wait before
         the completion list is updated. If not set the value of
         `timid-search-delay' is used. The function is called with
         no argument.


  Property: timid-pattern-function

  Value: Function to get the current search pattern. If not set the
         pattern is retrieved from the minibuffer. The function is
         called with no argument.


  Property: timid-candidates-variable

  Value: The variable containing the list of completion candidates. If not
         set then `minibuffer-history-variable' is used.


  Property: timid-visit-file-function

  Value: Function to visit the selected file. It is called with one
         argument, the path of the selected file. If not set then
         the file path is inserted into the minibuffer and the
         function `exit-minibuffer' is called.


timid.el is tested on Gnu Emacs 21. It is known to work with Gnu
Emacs 22. Compatibility with other Emacs versions is purely
coincidental. :)

Thanks to Drew Adams for his valuable suggestions on improving this
package.

Thanks to Vinicius José Latorre for customize support.
