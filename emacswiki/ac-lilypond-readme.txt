A file to set up the use of LilyPond with auto-complete which can be found
at http://www.cx4a.org/pub/auto-complete.el

Lilypond has functions to extract keywords etc from the lilypond-words.el file which in
turn is extracted by a python script from the code itself. I wish it had a list of the
lilypond scheme functions too.

Just add
(eval-after-load "LilyPond-mode" (load-library "ac-lilypond))
to your .emacs file. Happy auto-completing!
