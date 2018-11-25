`bookmark-open-in-symply-buffer' will generate buffer named
*Bookmark list*, which shows bookmarks from .emacs.bmk.
It has possibilities for deleting bookmarks from list,
saveing bookmarks list to file and loading  bookmarks list
from file.

Install.

(load "bookmark-add")
(global-set-key [?\C-c ?b ?l] 'bookmark-open-in-simply-buffer)
(global-set-key [?\C-c ?b ?m] 'bookmark-set-add)
(global-set-key [?\C-.] 'bookmark-jump-next-cyclic)
(global-set-key [?\C-,] 'bookmark-jump-prev-cyclic)
(global-set-key [?\C-'] 'bookmark-jump-backwards)

More commentary:

`bookmark-open-in-simply-buffer' - switch to buffer named
*Bookmark list*. If to hit key Enter on bookmark region will
pass on a corresponding file. If to hit key Delete (or Ctrl-d) on
bookmark region remove this bookmark from bookmarks list.
If to hit key 'q' the buffer will be closed.
`bookmark-set-add' - add this bookmark to bookmarks list.
To use history, it is necessary to press buttons Up and Down.
The first pressing Up inserts expression which is near to a point.
`bookmark-jump-next-cyclic' - cyclic moving on bookmarks forward.
`bookmark-jump-prev-cyclic' - cyclic moving on bookmarks backward.
`bookmark-jump-backwards' - will move to last cursor position right
 after uses of commands `bookmark-jump-next-cyclic' and
`bookmark-jump-prev-cyclic'.
