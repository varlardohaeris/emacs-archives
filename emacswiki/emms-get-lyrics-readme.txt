The function 'emms-get-lyrics-current-song' tries to
get the lyrics to the song that emms is currently playing.
It currently requires w3m to get the lyrics.
It copies the lyrics to a file ending in .lyrics; if the variable
`emms-get-lyrics-use-file' is nil, it will just display the lyrics
in a buffer without saving them to a file.
If the variable `emms-get-lyrics-dir' is non-nil, then the lyrics will
be put in this directory with the file ARTIST-TITLE.lyrics;
otherwise it will be put in the same directory as the song file, in
a file with the same name as the song file except the extension will
by ".lyrics".
