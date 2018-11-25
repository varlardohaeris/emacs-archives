Initialy based on dict-web.el by Eric Marsden (but now very different)

The definition come from the online dictionary at
http://www.francophonie.hachette-livre.fr
and is copyrighted by them.

Version History
1.0   Initial implementation
1.1   Cleanup and commented for gnu.emacs.source posting
1.2   Removed dependencies on w3
1.3   Some bad codes reworked
1.4   Some debuging.  Change in hachette-fetch-word-at-point
1.5   Addition of hachette-hexify-string to encode the url.
1.5.1 Quelques modification mineures.

1.6   Modification pour le cas ou mule n'est pas installe
      Changement de la regexp pour reconnaitre la notice de copyright.
      Support pour le serveur proxy.

1.7   Perfectionnement de la regexp pour font-lock
      Language selection for user interaction.

1.8   Put back w3 support (optional).
      w3 user can fetch definition asynchronously.
      Encore une modif a la regexp pour font-lock.
      Customize support.

1.8.1 Modification to the copyright notice
      (Thanks to Hrvoje Niksic)

1.9 Passage through 'checkdoc' to fix the package auto
     documentation.  Added the hash *hachette-memoize* to remember
     already fetched definitions.  (Thanks to Thomas F. Burdick)

1.10 add memorisation of the list of words (so there is no
      connection to the server if you search a word again).

1.11 Use iso2isosgml in emacs21 (old replace-from-table function
      keeped for backward compat) remove the prompt for killing the
      def window with 'q' but it may still be used by setting
      hachette-confirm-window-killing to 't'.  The y-or-n function
      may also be configured with hachette-yes-or-no-function




TO DO

+ Merge with other dictionary package to make it more generic?
+ Improve the language recognition.
+ Make the definition buffer prettier (some images, maybe?)


To use:

M-x hachette
Prompt for a word (suggesting word at point)

M-x hachette-fetch-word-at-point
Lookup word at point without prompting.
If hachette-use-language-guessing is t and
the buffer seem to be in english,
using dict-web is proposed.
