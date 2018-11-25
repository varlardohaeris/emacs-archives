This package provides a command and a minor mode which allow users
to scroll the window in the arbitrary direction by mouse dragging,
like "Grab and Drag" firefox extension. It's probably useful
for a touch panel based computer.

By default settings, you can do "Grab and Drag" using a command
`grab-and-drag' by mouse dragging with the left button. That means
grab-and-drag-mode overrides the binding of `down-mouse-1' event,
which is usually bound to `mouse-drag-region' command. Even Though
grab-and-drag-mode overrides the original binding, you can still use
it by holding down the button for 1 second without moving the pointer.

If you release the mouse button while moving the pointer, the
additional action is performed according to the dragging time and
direction. The mouse dragging longer than 500 milliseconds causes
an inertial scrolling vertically. Otherwise the quick dragging is
recognized as a flick gesture, which causes a scrolling by screenful
in that direction.

The code is rather X-dependent and is tested on GNU Emacs 22 and 23.


Installation:

First, save this file as grab-and-drag.el and byte-compile in
a directory that is listed in load-path.

Put the following in your .emacs file:

  (require 'grab-and-drag)
  (grab-and-drag-mode 1)

Then, start Emacs and grab-and-drag-mode is activated.

If you prefer using middle mouse button for scroll, put the following
instead of the above:

  (require 'grab-and-drag)
  (setq grab-and-drag-ignore-short-click t)
  (global-set-key [down-mouse-2] 'grab-and-drag)


Tips:

On low spec machine like ARM-based tablet, changing the pointer shape
might be extremely slow, so the slow response to clicking is annoying.
In that case, we can reduce the response time by disabling the change
of the pointer shape as:

  (setq grab-and-drag-pointer-shape nil)

If you want to use animated scroll commands included in this program
with keyboard operation, put the following to override the original
scroll commands:

  (defadvice scroll-up (around smooth-scroll-up activate)
    (if arg
        ad-do-it
      (grab-and-drag-scroll-up)))

  (defadvice scroll-down (around smooth-scroll-down activate)
    (if arg
        ad-do-it
      (grab-and-drag-scroll-down)))

  (defadvice scroll-right (around smooth-scroll-right activate)
    (if arg
        ad-do-it
      (grab-and-drag-scroll-right)))

  (defadvice scroll-left (around smooth-scroll-left activate)
    (if arg
        ad-do-it
      (grab-and-drag-scroll-left)))


History:
2010-11-28  S. Irie
        * Version 0.4.0
        * Add option `grab-and-drag-ignore-short-click'
2010-10-15  S. Irie
        * Version 0.3.2
        * Add functions to save original definitions of scroll commands:
           `grab-and-drag-orig-scroll-down'
           `grab-and-drag-orig-scroll-up'
           `grab-and-drag-orig-scroll-right'
           `grab-and-drag-orig-scroll-left'
        * Delete unnecessary local variables
        * Bug fix
2010-08-11  S. Irie
        * Version 0.3.1
        * Change to run hooks when dragging is finished at the ends of buffer
        * Bug fixes
2010-08-05  S. Irie
        * Version 0.3.0
        * Improve stability by considering heights of images
        * Bug fixes
2010-07-29  S. Irie
        * Version 0.2.0
        * Add Horizontal scrolling
        * Add Inertial scrolling
        * Add animated scrolling by screenfuls:
        * Add timeout of dragging
        * Add flick gesture recognition
        * Add hook variables:
           `grab-and-drag-beginning-of-buffer-hook'
           `grab-and-drag-end-of-buffer-hook'
        * Fix a lot of bugs
2010-07-21  S. Irie
        * Version 0.1.0
        * Initial version

ToDo:
