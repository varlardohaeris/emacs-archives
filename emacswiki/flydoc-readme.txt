To use flydoc, put the following code into your ~/.emacs:

(require 'flydoc)
(flydoc-default-setup)
(global-flydoc-mode 1)
(global-set-key "\C-ce" 'flydoc-explain)

TODO more general
TODO remove require
TODO documentation
TODO multi document support
TODO semantic support
TODO autoload
TODO hightlight (partially done)
TODO refactoring
TODO celdoc
