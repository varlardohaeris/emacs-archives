;;; Commentary:

;; This is two additions to `format-alist' for decoding
;;
;;    * ANSI SGR escape sequences "Esc [ ... m" colours, bold, underline,
;;      etc through ansi-color.el.
;;
;;    * Backspace overstriking for bold, underline, overline, and a bullet
;;      "+ backspace o".
;;
;; Such sequences are tty or line printer oriented output, but are sometimes
;; found in text files.  The aim is to make those files viewable and
;; hopefully have the attributes successfully copy into something like
;; `enriched-mode'.
;;
;; There's no automatic detection of these formats but you can "decode" a
;; buffer containing them with
;;
;;     M-x format-decode-buffer backspace-overstrike
;; and/or
;;     M-x format-decode-buffer ansi-colors
;;
;; `format-decode-buffer' has completion when it prompts for a format name.
;;
;; See `tty-format-guess' below for an idea to automatically notice text
;; files using these formats.
;;
;; Groff produces output like this (via grotty), and some of its manuals
;; have both ANSI and backspacing, as do various other packages with text
;; files produced from roff input.  You might think backspacing by now would
;; be as long gone as the teletypes it was made for, but grotty still uses
;; it creatively.
;;
;; Groff actually has lots of character overstrike sequences to make ink
;; resembling non-ASCII characters.  There's far too many to want in the
;; code here -- you're much better off asking groff for extended charset
;; output in the first place (utf8 or whatever), instead of decoding bizarre
;; combinations after the fact.  So the aim here is only to handle bits
;; found in real life documents.  One moderately frequent bit not yet
;; supported is | plus = for a footnote dagger.
;;
;; See also underline.el for a couple of simple functions adding or removing
;; backspace underlining.

