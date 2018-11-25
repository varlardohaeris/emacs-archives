After activating the minor mode "msearch-mode" mouse-dragging over
some text highlights all matches of this text in the current
buffer. Msearch-mode can be (de)activated by (un)checking msearch
in the minor-mode menu of the mode line.

Installation:
Put msearch.el into your load-path and add the following line into
your emacs start-up file (e.g. "~/.emacs"):
(require 'msearch)


Changes:

2010-06-22, 18:45, TN:

Error: Infinite recursion of msearch-event-handler at re-activation
of msearch-mode.

Fix: Reset event handler by (local-unset-key (kbd
"<drag-mouse-1>")) rather than by (local-set-key (kbd ...)
msearch-next-event-handler).

2010-06-23, 23:00, TN:

Error: msearch-event-handler didn't call msearch-next-handler. Thus
mouse-set-region was not called.

Fix: Return value 't of (msearch-next-handler-ok).

Feature: User can remove all highlights by dragging a region of zero length.

Implementation: Allow msearch-word of zero length.

2010-06-24, 22:00, TN + SCZ:

local-set-key is not buffer-local but major-mode local. Thus msearch-event-handler has been activated in all buffers with the same major-mode but local variables were missing.

Fix: Introduced new mouse event handler for <drag-mouse-1>.

2010-07-25, 22:00, TN:

Added msearch-enslave-buffer.

2010-07-26, 22:00, TN:

Added minor-mode-menu.

2010-07-27, 18:40, TN:

Better menu and defaults for msearch-enslave-buffer and
msearch-release-buffer.

2010-08-19, 0:0, TN:
Added menu button for explicitely setting msearch-word.

2011-03-10, 16:15, TN:
Added custom-variable msearch-max-length and avoid crash through
too long msearch words.

2013-08-22, TN:
Freezing of highlights, swtich on msearch for the window with event posn

2013-10-23, TN:
Optional msearch for double-click and dragging.

2013-10-29, TN
Include proposal of Peter Harlan (Option case-fold-search)
word/symbol limits

2013-11-09 TZA
Implement most of Peter Harlan's proposals.

2013-11-14 TZA
Customization for radio button "Word Mode".
