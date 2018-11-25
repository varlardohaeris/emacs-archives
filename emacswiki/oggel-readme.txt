Commentary:
There seems no good tool in Emacs (except eshell :) to encode a cd
or single tracks to the great ogg-vorbis format.  Although there
are similar tool for mp3s like Monk or 'The Idiot Jukebox' i
decided to write my own major mode for that task as i find it
pretty hard to work through their code and understand it (as both
are very complex and not just ripping tools) and i always wanted
to program my own mode and learn from that.  Because of that you
will find a lot of little (or greater :) bugs and nasty code
here.  Feel free to work through the code and change everything you
want, send me a patch or a bugreport.
To install the code put it simply in your load-path and write that
piece in you .emacs:

(add-hook 'cdi-mode-hook (lambda ()(require 'oggel)))

Now you just type M-x cdi and gets to the tracklist.  You can
mark you favourite tracks with "m" and press "C" to start encoding.  If you
don't mark any track oggel will encode the whole cd for you! Yeah!
The files will be produced with the file name format
artist-album-tracknumber-songtitle.ogg unless you change
oggel-filename-list.  The standard options should work well on all
normal machine configurations (machines like mine).

Oggel bases heavily on the great mode cdi.el from Matt Hodges for
playing the tracks or produce the tracklist.  You can get it from:
http://www.emacswiki.org/elisp/cdi.el
