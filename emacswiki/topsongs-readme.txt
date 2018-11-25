 TopSongs provides a song database for the lazy emacser. TopSongs interacts
 with both EMMS and mp3info, in order to provide handy commands such as
 `topsongs-insert-current-selected-track', recording your favorite songs
 on-the-fly. Next it will generate for you a m3u playlist, playable with
 `topsongs-play' in any moment.

 For a full-featured playlist generation don't forget to install mp3info.

Installation:

 Put this file on your Emacs-Lisp load path, then add the following to your
 ~/.emacs startup file.

    (require 'topsongs)
