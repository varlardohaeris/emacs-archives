You can bookmark a buffer path, and can call it with regex match.
You can open file without dired buffer open

Installation

Add bpath.el to your load path
add your .emacs

(require 'bpath)
(define-key global-map "\C-c\C-b" 'bpath::book)
(define-key global-map "\C-c\C-z" 'bpath::read)

Usage

1.To save current path

    C-c C-b

2.To call saved path

    C-c C-z
