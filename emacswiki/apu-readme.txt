   Apropos Unicode characters.

 Command `apropos-char' (aka `apropos-unicode', aka `apu-chars')
 describes the Unicode characters that match an apropos pattern you
 specify: a regexp or a space-separated list of words.  The
 characters whose names or old names match are shown in a help
 buffer, along with the names (or old names) and code points
 (decimal and hex).

 Command `describe-chars-in-region' (aka `apu-chars-in-region')
 describes the Unicode characters that are in the region.  By
 default, it shows each distinct character only once.  With a
 prefix argument it has a line describing each occurrence of each
 character in the region.

 For each of these commmands, in the help buffer describing the
 characters you can use these keys to act on the character
 described on the current line:

  * `RET' or `mouse-2' - see detailed information about it.
  * `^' - insert it in the buffer where you invoked
          `apropos-unicode'.
  * `c' - define a command to insert the character, having the same
          name.  (You need library `ucs-cmds.el' for this.)
  * `i' - google for more information about it.
  * `k' - globally bind a key to insert it.
  * `l' - locally bind a key to insert it.
  * `z' - show it in a zoomed tooltip.
  * `C-y' - copy it to the `kill-ring'.
  * `M-y' - copy it to the secondary selection.

 You can sort the list of matches by any of the columns, up or
 down, by clicking its heading.

 For command `apropos-unicode', you can use options
 `apu-match-words-exactly-flag' and
 `apu-match-two-or-more-words-flag' to specify your preference for
 the kind of word matching to use by default.  You can match each
 word or only any two or more words.  If matching each word, you
 can match them as substrings or as full words.  You can use `C-c
 n' to refresh the matches, cycling among these word-match methods.

 Non-`nil' option `apu-match-only-displayable-chars-flag' means
 that commands such as `apropos-unicode' display only Unicode chars
 that can be displayed in the current context.  Non-displayable
 chars are those that do not have displayable glyphs, in general,
 and those for which you do not have a font installed that can
 display them.  Displayability of a character is determined by
 standard Emacs function `char-displayable-p'.

 NOTE:

   Starting with Emacs 25, `char-displayable-p' can be EXTREMELY
   SLOW if the new variable `inhibit-compacting-font-caches' is
   `nil', which it is by default, and if you have many, or large,
   fonts installed.  For this reason the default value of
   `apu-match-only-displayable-chars-flag' is `nil' for Emacs 25
   and later.  This seems to be the case if you have MS Windows
   TrueType fonts installed, for instance.

   If you want to be able to exclude non-displayable chars in Emacs
   25+, then set `apu-match-only-displayable-chars-flag' to
   non-`nil'.  If you find that this makes APU commands such as
   `apropos-char' extremely slow then set variable
   `inhibit-compacting-font-caches' to `t'.


 Commands defined here:

   `apropos-char', `apu-chars-in-region', `apropos-unicode',
   `apu-char-codepoint-at-point', `apu-char-name-at-point',
   `apu-chars', `apu-chars-matching-full-words',
   `apu-chars-matching-two-or-more-words',
   `apu-chars-matching-words-as-substrings', `apu-chars-narrow',
   `apu-chars-next-match-method', `apu-copy-char-at-point-as-kill',
   `apu-copy-char-here-as-kill',
   `apu-copy-char-at-point-to-second-sel',
   `apu-copy-char-here-to-second-sel', `apu-define-insert-command',
   `apu-global-set-insertion-key', `apu-google-char',
   `apu-local-set-insertion-key', `apu-mode',
   `apu-chars-refresh-matching-as-substrings',
   `apu-chars-refresh-matching-full-words',
   `apu-chars-refresh-matching-two-or-more-words',
   `apu-chars-refresh-with-next-match-method', `apu-revert-buffer',
   `apu-show-char-details', `apu-zoom-char-here',
   `apu-zoom-char-at-point', `describe-chars-in-region'.

 User options defined here:

   `apu-match-only-displayable-chars-flag',
   `apu-match-two-or-more-words-flag',
   `apu-match-words-exactly-flag', `apu-synonyms'.

 Non-interactive functions defined here:

   `apu-add-to-pats+bufs', `apu-buf-name-for-matching',
   `apu-char-at-point', `apu-char-displayable-p', `apu-char-here',
   `apu-char-name', `apu-char-names', `apu-char-name-here',
   `apu-char-string-here', `apu-chars-narrow-1',
   `apu-chars-read-pattern-arg', `apu-compute-matches',
   `apu-copy-char-to-second-sel', `apu-filter',
   `apu-full-word-match', `apu-get-a-hash-key',
   `apu-get-hash-keys', `apu-hash-table-to-alist',
   `apu-make-tablist-entry', `apu-match-type-msg',
   `apu-print-apropos-matches', `apu-print-chars',
   `apu-remove-if-not', `apu-sort-char', `apu-substring-match',
   `apu-tablist-match-entries'.

 Internal variables defined here:

   `apu--buffer-invoked-from', `apu-latest-pattern-set',
   `apu--matches', `apu--match-two-or-more', `apu--match-type',
   `apu--match-words-exactly', `apu--orig-buffer',
   `apu--pats+bufs', `apu--patterns', `apu--patterns-not',
   `apu--refresh-p', `apu--unnamed-chars'.
