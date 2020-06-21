This program extract big words from text.
The words whose Zipf frequency less than `mybigword-upper-limit' are
big words.

Zipf scale was proposed by Marc Brysbaert, who created the SUBTLEX lists.
Zipf frequency of a word is the base-10 logarithm of the number of times it
appears per billion words.

A word with Zipf value 6 appears once per thousand words,for example, and a
word with Zipf value 3 appears once per million words.

Reasonable Zipf values are between 0 and 8, but the minimum Zipf value
appearing here is 1.0.

We use 0 as the default Zipf value for words that do not appear in the given
word list,although it should mean one occurrence per billion words."

Thanks to https://github.com/LuminosoInsight/wordfreq for providing the data.

Usage,

  Run `mybigword-show-big-words-from-file'
  Run `mybigword-show-big-words-from-current-buffer'


Customize `mybigword-excluded-words' or `mybigword-personal-excluded-words' to
exclude words.

Customize `mybigword-default-format-function' to format the word for display.
If it's `mybigword-format-with-dictionary', the `dictionary-definition' is used to
find the definitions of all big words.

Customize `mybigword-hide-word-function' to hide word for display
