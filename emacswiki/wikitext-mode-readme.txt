This is `wikitext-mode', a major mode for editing articles written
in the markup language used by Wikispace. It is intended to work
with GNU Emacs 21.x, though it may also work with other versions of
(X)Emacs.

Installing wikitext-mode
=========================

Save wikitext-mode.el in a convenient directory, preferably in
your `load-path'. Add the following to your `user-init-file':

  (autoload 'wikitext-mode
    "wikitext-mode.el"
    "Major mode for editing wiki-documents." t)

If you did not save wikitext-mode.el in your `load-path', you must
use the full pathname. On MS Windows, use forward slashes (/)
rather than back slashes (\) to indicate the directory, e.g.:

  (autoload 'wikitext-mode
    "C:/Documents and Settings/USERNAME/.emacs.d/Wikitext-mode.el"
    "Major mode for editing wiki-documents." t)

If you want to associate filenames ending in ".wiki" with
wikitext-mode, add the following to your init file:

  (setq auto-mode-alist
    (cons '("\\.wiki\\'" . wikitext-mode) auto-mode-alist))

Installing longlines-mode
=========================

Wikitext articles don't use newline characters to break paragraphs
into lines, so each paragraph looks like a super-long line to
Emacs. To let Emacs handle "soft word wrapping", you need to
download a third-party package, longlines-mode.

Download longlines.el, saving into your `load-path':

  http://www.emacswiki.org/elisp/longlines.el

Add the following to your `user-init-file':

  (autoload 'longlines-mode "longlines.el"
    "Minor mode for editing long lines." t)

TODO:
+ do not reformat headers
* use monospace face inside [[code]]...[[code]] (just place a space at the beginning of each line)
+ do not touch the text inside [[code]]...[[code]]
- make regexps customizable
- make customization group
- clean up the code
