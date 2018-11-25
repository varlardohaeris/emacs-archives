Show context-aware commands by `anything'. It avoids forgetting
existence of appropriate commands. KYR is an abbreviation of "Kuuki
wo YomeRu", which means that person can read the atmosphere in
Japanese. Anything-kyr shows appropriate commands for the
situation.

Note that anything-kyr.el provides only the framework. See
anything-kyr-config.el for practical, polished, easy to use
configurations which can be used to assemble a custom personalized
configuration.

http://www.emacswiki.org/cgi-bin/wiki/download/anything-kyr-config.el


Installation:

Put anything-kyr.el and anything-kyr-config.el to your load-path.
The load-path is usually ~/elisp/.
It's set in your ~/.emacs like this:
(add-to-list 'load-path (expand-file-name "~/elisp"))

And the following to your ~/.emacs startup file.

(require 'anything-kyr-config)

Then put `anything-c-source-kyr' to `anything-sources'.
M-x `anything-kyr' for demo.
