Commentary:

Added TextMate wrapping of selection:

      It is also possible to wrap a selection in an open/close
      character by selecting text and typing the opening
      character. For example if you type foo, select it and type (
      then TextMate will make it (foo) placing the caret after the
      ending parentheses.

This requires `autopair-autowrap' to be true.

The default TextMate method of putting the cursor at the end
parenthesis is useful in many modes (though not really lisp).  To
change where the cursor ends up you can customize
`autopair-goto-char-after-region-wrap'.  It may be placed in the following locations:

1(2foo3)4

1 -- Before the open parenthesis
2 -- After the open parenthesis (useful for lisp)
3 -- Before the close parenthesis
4 -- After the close parenthesis (TextMate default)

Anything else puts the point where you had it .  To change the
default after-location for a certain mode, like emacs-lisp-mode,
you may wish to add the following hook to emacs-lisp-mode:

(add-hook 'emacs-lisp-mode-hook (lambda() (setq autopair-goto-char-after-region-wrap 2)))

This only works outside of comments and strings.  Otherwise the
value of `autopair-goto-char-after-region-wrap-comments'.

In a similar vein, though I'm unsure it is supported in TextMate, I
added wrapping of a quote.  When
`autopair-escape-region-when-quoting' (enabled by default) is true,
then it will appropriately quote the string.  For example selecting
the following string:

This is a test of the quoting system, "this is only a test"

And pressing quote, gives:

"This is a test of the quoting system, \"this is only a test\""

 Quoting the whole phrase again gives:

"\"This is a test of the quoting system, \"this is only a test\"\""

You may also select a region within the string and quote it.  When
this happens, the appropriate escape characters are used.  For
example by selecting the word quoting and pressing " in
emacs-lisp-mode gives:

"\"This is a test of the \"quoting\" system, \"this is only a test\"\""

Note:  For this to work font-locking must be enabled.

Therefore already quoted strings will not be quoted again.

This behavior is appropriate for most modes.  By default, the
escape character is determined by the syntax class (looks at first
256 characters)

However if this is visual basic mode, you may prefer the following:

"This is a test of the quoting system, ""this is only a test"""

Also upon re-quoting the above region, one gets:

"""This is a test of the quoting system, ""this is only a test"""""

This may be achieved by changing the `autopair-quote-string' to
"%s" by the following hook:

(add-hook 'visual-basic-mode-hook (lambda() (setq autopair-quote-string "%s")))

Currently quoting only occurs outside of comments.

Also when typing inside a string, quotes are generated automatically.  When
typing twice, the quoted quotes are change to regular quotes.

In addition to having  a backspace change:
 " \"|\" "
to
"  "
It also changes:

  " \"\"| "

To
 " \" "

unless `autopair-delete-orphan-quoted-string' is nil

Added `autopair-skip-whitespace-but-dont-delete'.  This will skip
whitespace, but not delete it.  This implies the following:

In R mode, lets say you type:

   f <- function(x,...){

If | represents the carat, autopairs will give you the following:

  f <- function(x,...){|}

By typing return the following is produced

 f <- function(x){
     |
 }


Currently in autopair mode the effect of typing } gives:

 f <- function(x){
     }|
 }

By enabling `autopair-skip-whitespace-but-dont-delete', typing a }
gives:

f <- function(x){

}|

Features that I call bugs that are taken care of with this
modification: Deleting when autopairs shouldn't and special returns
when autopairs shouldn't.  This can be customized by
`autopair-backspace-fix'.  I'm not sure which method TextMate
actually does.


Consider the following in lisp:

()|()

If you backspace, then the following occurs

()

I believe that this should occur instead:

(()

With the first expression:
()|()

Pressing return yields
 ()
 |
 ()
I believe it should yield:

()
|()
