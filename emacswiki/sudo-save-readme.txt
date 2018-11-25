Use `write-file-hooks' and `after-save-hook' to run "sudo chown ... " before
AND after file save.  This allows the Emacs process to grant ownership to the
user and then restore ownership just after save.

TODO:

- Actually what we SHOULD do is actually do a chown this way and NOT a chmod.

- Can sudo cache passwords?  I think it can.

- Ability to chmod a+r a file JUST prior to reading it ... and then restoring
  permissions RIGHT after it.

- Ability to s
