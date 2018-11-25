Sometimes I record a small keyboard macro just to run some simple
command over and over again, each X number of seconds, often to
execute some SQL query or running some other command to monitor
some process.  It is easy enough to execute the macro with some
specially bound key, but why not letting Emacs do it for me
instead?  That's why I made this small hack.  Another example of
usage could be to have it act as a simple document reader; just
record a keyboard macro to step one line down, and then start the
timer.

To install it, put the file in your load path and add these lines
to your .emacs:

  (autoload 'keyboard-macro-timer-start "keyboard-macro-timer"
  "Execute last keyboard macro with a timer." t)

To use it, call `keyboard-macro-timer-start' after you have
recorded a keyboard macro.  You will be prompted for a number of
seconds to wait before executing the macro the first time and for
the interval, in seconds, between invocations.  Both values may be
integers or floating point numbers so you can specify fractions of
a second and both have default values
(`keyboard-macro-timer-default-start-in' and
`keyboard-macro-timer-default-interval') so that you can just type
`RET' to accept the defaults.  The default values are configurable.

To stop the execution, call `keyboard-macro-timer-cancel'.

Make it really easy to stop the timer by binding the cancel command
to a key:

 (global-set-key [f6] 'keyboard-macro-timer-cancel)

Try it now: type `C-x (' to start a keyboard macro, then type `C-f'
to step one character forward.  Stop macro recording with `C-x
)'. Next, do M-x keyboard-macro-timer-start and type `RET' at both
prompts.  Stop it the timer by running
`keyboard-macro-timer-cancel'.

Only tested on Emacs 22.1.
