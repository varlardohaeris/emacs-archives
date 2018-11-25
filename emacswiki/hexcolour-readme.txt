This package provides a minor mode for highlighting of HTML-style
colour specifications, a là #324f3a. "Fine," you say, "what's so
special about this?" Well, when this mode is turned on, Emacs
highlights the colour spec _in the specified_ colour (as
background).

To activate it, put it into a directory in you `load-path' and put
this into your .emacs:

(autoload 'hexcolour-mode "hexcolour" nil t nil)

(add-hook <your favourite major mode hook>
	  (lambda ()
	    (hexcolour-mode 1)))

This package was implemented as a minor mode in order to work
around some quirks in the implementation of font-lock in the
current (as of summer 2003) released version of GNU Emacs. If you
use a recent CVS version of Emacs (the one that will probably (!)
become 21.5), then you shouldn't use it. You can simply something
like this into your .emacs:

(defvar hexcolour-keywords
  '(("#[abcdef[:digit:]]\\{6\\}"
     (0 (put-text-property (match-beginning 0)
			   (match-end 0)
			   'face (list :background
				       (match-string-no-properties 0)))))))

(defun hexcolour-add-to-font-lock ()
  (font-lock-add-keywords nil hexcolour-keywords))

(add-hook <your favourite major mode hook> 'hexcoulour-add-to-font-lock)
