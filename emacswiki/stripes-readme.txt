highlights every even line with an alternative background
color. Useful for buffers that display lists of any kind - as a
guide for your eyes to follow these lines.

when invoked with a numeric prefix arg, color that many lines
instead of every other line.

Put this file into your load path,
(require 'stripes) and do a (turn-on-stripes-mode)
 whenever you need this (e.g. in hooks).

Changelog

--- Version 0.2 (2003-11-01)

- added autoload cookies
- added turn-on method (for hooks)
- prefix arg specifies how many lines to treat as one line
- renamed the file from stripes-mode.el to stripes.el to
  make the name fit into the Emacs file naming conventions
- widen all restrictions when refreshing

--- Version 0.1 (2003-10-02)
