Given the number of acronyms associated with emacs and gnu it is
perhaps surprising that this idea for the dynamic expansion of
acronyms is not already available in emacs.

This provides a method to type in acronyms and have them expanded
in place. Its interface is similar to that of `dabbrev'. That is,
an expansion of the acronym is looked for in the current buffer
then other open buffers and if the expansion found is not what you
want then calling `magpie-expand' again immediately rejects the
present expansion and replaces it by a new one if such exists. It
also includes ordinary dabbrev expansion. If there is no expansion
of the acronym left it will try to expand prefixes of your acronym
whilst retaining the unexpanded part of your acronym for later
re-use.

To be more precise, we type a string which takes the following
form. The first character is often a control character which
determines how the remaining characters should be transformed into
a regexp. The next few characters are alphabetic and these are
transformed by a recipe determined by the control character into a
regexp of some sort. Finally, the last character or characters may
be a number which is simply how many extra words should be glued
on the end. Thus in this buffer ",pam4" could expand to "provides
a method to type in acronyms" as would "prov6". The absence of a
control character means that the expansion should be of `dabbrev'
type and if the control character is "," then an acronym expansion
should be used whilst if the control character is "." then an
acronym expansion method is used but the text should begin inside
a mathematics environment in a LaTeX document. For example,
".EEpAB" might expand to "\\Ext_{E^{\perp}}(A,B)". This
illustrates other properties of the expansion method which is that
the capitalization of the acronym determines the capitalization of
the inserted string and also that the inserted string should
consist of balanced sexps (balanced on the right but not
necessarily on the left) if possible; in this particular case this
would mean that ".EEpA" would also expand to the same phrase since
the regexp match would include the first bracket "(" and the
closure must then contain "(A,B)". There is a further wrinkle
which is set up specifically for typing LaTeX. The string ",aat/i"
will expand to "and assume that $$ is" with the cursor between the
two dollar signs if this phrase (with something between the two
dollar signs) occurs in the buffer. After typing here, "\C-."
takes the cursor to the end of the inserted phrase. Finally, let
us suppose that we type ",aattkifp" for which there is no
expansion. Depending on the buffer contents, we may find that this
is expanded to "and assume that ,tkifp" with the cursor at the
space before the comma. If "and assume that" is not what we want
then calling a`magpie-expand' will attempt to replace it; if it is
what we want then `magpie-complete-expand' will attempt to expand
the rest; for example we might get "and assume that the \kalg is
finitely presented". I have provisionally bound `magpie-expand' to
"\M-n" and `magpie-complete-expand' to "\M-N". However, for
myself, I have bound them to "\H- " and "\A- ".

one final wrinkle when typing LaTeX. If we call `magpie-expand' just
after a $, it simply inserts the contents of the previous inline
math environment that has length at least 6 and further calls to
`magpie-expand' replace this by steadily more distant strings of the
same type. If we call `magpie-expand' just after _{ it inserts a
previous subscript of length at least 6 and if we call it after ^{
we get the same for superscripts. Finally if we call it just after
{ not preceded by either _ or ^, it finds the contents of a
previous pair {} provided that this is not a sub- or superscript.

It is worth being precise about what an acronym
matches. Specifically words are strings of alphabetic characters
(for me this is [a-zA-Z]+ but this is customizable: see
`magpie-alphabetic-chars') separated by non-alphabetic characters.

I have also done similar things for editing Emacs Lisp files. Here
again acronyms can be very useful. So I have set things up so that
the control character "," introduces an acronym for strings from
the buffer in a similar way to the above (using the same
convention for words as in the previous paragraph). However, the
control character "." introduces an acronym for symbols where
words as above are separated by hyphens. Thus ".bdcu" expands to
"backward-delete-char-untabify". This not only searches the
current buffer but also looks at all symbols. This way of
searching for names of symbols is substantially quicker than
standard completion and I have also written functions below that
incorporate this method (actually a related one) called
`magpie-execute-extended-command', `magpie-describe-function' and
`magpie-describe-variable' which begin by asking for a partial
acronym for the command, function or variable you want (thus "bdc"
matches "backward-delete-char" but it also matches
"backward-delete-char-untabify") and then use a standard
completion mechanism if there is more than one match. The reason
for using prefix acronyms rather than full ones is that one may
not remember the full name and this allows one to find it via the
usual completion mechanism if one knows how the symbol's name
begins.

The system is highly extendable. As an illustration let us suppose
that we wanted to include the ability to replace a regexp reg by
some expansion from the buffer of the regexp. We choose a control
character, let us say, ?;, and we add the association (cons ?;
magpie-any-regexp) to `magpie-control-chars-alist'. The reader should
then follow through the recipe for constructing `magpie-any-regexp'
along the lines of `magpie-acr-template'. After this has been done
then the string (concat ";" reg "4") would expand to a match for
the regexp reg followed by the next 4 "words".

The method is rather general because first we construct some
regexp and find a match for it. Then we may adjust precisely which
buffer-substring we use programmatically. We then apply a further
function to compute what string we are goint to insert. Finally we
have control over what we do when we insert the string (which is
what allows us to set up a simple template mechanism).

Things to do: Add further methods for constructing useful
regexps. Think about ways to store past expansions though I am not
convinced that this is wholly useful and it is not the way dabbrev
operates.

Variables

Customizable variables
