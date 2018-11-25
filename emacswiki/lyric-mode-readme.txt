Edit lyric files, with playing of the music file at a controlled
speed and recording of the synchronization information.

This uses the external program `ogg123'.  Potentially, it could use
other, similar, programs for playing other types of music file.  It
assumes that the music file and the lyric file have the same
basename, e.g. another-gnu.ogg and another-gnu.lrc

If the file is already marked up with synchronization tags, an
overlay is displayed as the song is played.

Mostly, the mode is controlled by C-c C-whatever, but while the
song is playing, some other keys are redefined for easy capture of
the synchronization information.  The space bar inserts a
synchronization tag at point, and moves to the start of the next
line, and the return key moves to the start of the next line.
Lines matching `lyric-mode-skip-lines' (normally, blank lines) are
skipped by the space bar, so you get through marking a song with
just the space bar if there are no complications.

When point is on a tag, you can nudge the tag time backward or
forward by half a second using `<' and '>'.
