Some code for interacting with spamprobe from within RMAIL. This code
assumes that you have a functional mailsystem with procmail calling
spamprobe and inserting an X-SpamProbe header in filtered messages
before the messages reaches RMAIL.

Making it so is left as an exercise to the reader.  The spamprobe
documentation has pointers on how to do this.

The single entry point is `rmail-spamprobe-retrain-dwim', which will
examine the message for its current score, have spamprobe reverse it
and then send off the message to procmail again for a new round of
processing.

For ease of use, binding this function to a key in `rmail-mode-map'
and `rmail-summary-mode-map' would make sense.  I also suggest adding
"x-spamprobe" to the list of displayed headers in
`rmail-displayed-headers'.
