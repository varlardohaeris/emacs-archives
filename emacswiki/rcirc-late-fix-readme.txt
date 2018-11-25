The idea is to detect messages like 's/tset/test' and overwrite the
wrong word with the correction, on the original phrase, using
overlays.

Please mail me (hugows@gmail.com) about any improvements or bug
reports you have regarding this file.

Changes (16/07/2011) by Elias Pipping
[x] Use rcirc-response-formats for building the name string
[x] Support s/word// and s/word//g for removing matching text
[x] s/a/b/g bug for input "aaaaaa" fixed

plus some cleanup/improvements

Changes (31/01/2011) Fix two warnings

Changes (11/12/2007) (Tks tsdh for the suggestions)

[x] Exclude the nicknames from correction.
[x] Strip the / from the replacement text.
[x] With s/foo/bar the last occurence of foo is replaced with bar, not the first.
[x] Support s/foo/bar/g to replace all occurences of foo with bar.
[ ] Use rcirc-response-formats for building the name string
[ ] A variable could control whether only whole words are changed (then
just concat "<" word ">")
[ ] Support s/word// and s/word//g for removing matching text
