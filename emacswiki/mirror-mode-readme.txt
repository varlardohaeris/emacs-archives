Provides a simple minor mode to insert parens and string quotes in pairs.  It
can mirror the following keys: { } ( ) [ ] < > " '

Set `mirror-keys-to-bind' if you want to limit which keys are setup as mirror
keys.  In most cases you'll also want to make this a local variable first.
Use (mirror-mode) to toggle the mode.  If `mirror-wrap-region' is non-nil
(the default) then typing a mirror key when the region is active will wrap
the region in the appropriate characters.

Much of this code is ripped from js2-mode.
