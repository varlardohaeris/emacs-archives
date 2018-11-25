Pushy completion is a variation of timid completion. It's timid
completion without the timidity.

When using a command with minibuffer history, completions from the
minibuffer are offered automatically if the user hesitates a bit
(configurable).

If there are possible completions then pushy completion displays
them and the user can select a match with the C-UP/DOWN keys and
C-ENTER. If more characters are typed the list of matches is
updated. Pushy completion list can be dismissed with ESC.

Pushy completion can be enabled with

	(pushy-mode t)

This enables pushy completion mode, but you also have to specify
which commands you want to have affected by pushy.

Pushy completion can be turned on for all commands by customizing
variable `pushy-enable-globally'. Alternatively, it can be enabled
for individual commands by putting property `pushy-completion' with
value `enabled' on the command. Also, it can be enabled globally
and be disabled for individual commands by setting value of
property `pushy-completion' to `disabled' for the command. See
below for a complete list of possible command properties.

Pushy completion can also offer items which are outside of the
current command's scope. For example, iswitchb can offer
completions from the file history. This way iswitchb can be used
for switching buffers, but also for quickly revisiting a previously
opened file, so there is no need to use a separate command for
it. According to my experience it is a very convenient and
efficient setup.

Iswitchb requires special treatment, so use this command to enable
pushy for it:

	(pushy-iswitchb-setup)


In order to take full advantage of pushy history completion use of
package savehist is recommended with a sufficently high
history-length value (I use 1000).


Complete list of command properties for pushy completion:

  Property: pushy-completion

  Values:

    enabled - Pushy is enabled for commands having this
              property. This is not necessary if you enabled pushy
              globally via variable `pushy-enable-globally'.

    disabled - Pushy is disabled for this command. This is useful
               if you have enabled pushy globally via variable
               `pushy-enable-globally', but want to inhibit it for
               certain commands.


  Property: pushy-search-delay-function

  Value: Function to determine how much should timit wait before
         the completion list is updated. If not set the value of
         `pushy-search-delay' is used. The function is called with
         no argument.


  Property: pushy-pattern-function

  Value: Function to get the current search pattern. If not set the
         pattern is retrieved from the minibuffer. The function is
         called with no argument.


  Property: pushy-candidates-variable

  Value: The variable containing the list of completion candidates. If not
         set then `minibuffer-history-variable' is used.


  Property: pushy-visit-file-function

  Value: Function to visit the selected file. It is called with one
         argument, the path of the selected file. If not set then
         the file path is inserted into the minibuffer and the
         function `exit-minibuffer' is called.


pushy.el is tested on Gnu Emacs 22.
