Emacs supports regular expression and matching function.
One interesting feature is that you can refer to the m-th matched
subexpression specified by \(..\) as \m.
Although this feature is useful, it does not seems to me very
convenient because you have to count the subexpressions.
It is error prone especially when the regular expression
becomes complicated.

What I wanted to realize in this package is to extend the above
feature so that you can NAME each subexpression.

This package provides you a simple text rewriting system
in which you can write a rewriting rule
using matched subtext with names.

The basic idea is the following.
(i)  We use a sequence of strings instead of a single string of
     regular expression.
(ii) Toghether with a suitable delimiter these strings
     are concatenated.
(iii)We can name a string by parenthesizing it. The name is a symbol
     at the 'car' part of the parenthesized string.
(iv) The name given in (iii) can later be used to refer to the
     matched string.

For example the following list represents a line of text
beginning with "Your response:" and the symbol 'response' matches
any subsequent text before a newline (and after a suitable delimiter
whose default value is 'white' i.e., space or tab):

   ("Your response:" (response ".*"))

Then you can use the symbol 'response' to refer to the matched string
for later text rewriting.
In other words the above list matches the same text as the string
"Your response:[ \t]*\(.*\)" does. And you can refere to the part of
the text matching ".*" by the name 'response' instead of '\1'.

Rewriting System:
After loading this package, you can use the rewriting system
via 'rewrite-prog' macro. Inside this macro you can use the commands
as follows.
(rewrite-prog <command>*)
where
 <command> ::= (search <regular expr seq> <search options>?)|
               (replace-match <newtext seq> <replace options>?)|
               (rewrite <regular expr seq> <result seq>)|
               (call <s-expression>*)|
               (bind (<variable name> <s-expression>)*)|
               (if <condition> <s-expression> <s-expression>?)|
               (while <condition> <s-expression>*)|
               (progn <s-expression>*)|
               (update <variable name> <s-expression>)
               (insert . <regular expr seq>)

Definition (Environment):
The rewriting system maintains pairs of a symbol and the string
matched with it. We call these pairs the environment of the system.

Commands:
Currently the rewriting system accepts the following commands:
  search  -  syntax: (search <regular expr seq>
                             <var-alist> <limit> <noerror> <repeat>),
             where the last four arguments are optional.
     This command searches the given regular expression sequence
     <regular expr seq>. If a matching is found, it updates the
     environment.
     The behavior when the matching is not found can be specified
     by one of optional arguments
     which are the same as those of 're-search-forward.'
     Its return value is also similar to 're-search-forward'.
  replace-match - syntax:
                   (replace-match <newtext seq> <fixedcase> <literal>)
     This command replaces the matched text in the same manner as
     the lisp function of the same name 'replace-match'.
     The difference is that the first argument <newtext seq>
     is not a singlestring but a list of strings or
     symbols refering to previously matched subexpressions.
     This command alwasy returns 'nil'.
  rewrite - syntax: (rewrite <regular expr seq> <newtext seq>).
     This command rewrites the text in the buffer matching
     <regular expr seq> into <newtext seq>, which is a sequence of
     strings or symbols representing matched subtext.
     It always returns 'nil'.
     This command is equivalent to
          (progn (call (beginning-of-buffer))
                 (while (search <regular expr seq> nil t nil)
                        (replace-match <newtext seq> t t))).
  call    - syntax: (call <s-expression>*).
     This command first substitutes the matched strings
     appearing in <s-expression>'s into the named symbols
     and then evaluate them.
     It returns the value of the last <s-expression>.
  bind    - syntax: (bind ((<variable name> <s-expression>)*)).
     This command adds a new biniding to the environemt.
     You can write <s-expression> to create a string
     or an integer to be bound to <variable name>.
     This command always returns 't'.
  if      - syntax: (if <conditional command>
                         <then command> <else command>),
             where <else command> is optional.
     This command first interpretes <conditional command>
     under thecurrent environemt. If the return value of <command>
     is not nil, <then command> will be executed.
     Otherwise <else command> will be.
  while   - syntax: (while <conditional command> <command>*).
     This command first interpretes <conditional command>
     under the current environment. If the return value of
     <conditional command> is not nil,
     then evaluate the series of <command>'s.
     It will repeat this procedure while <conditional command>
     is not nil.
  progn   - syntax: (progn <command>*).
     This command simply interpretes each <command> in turn.
     It is used to turn a series of <command>'s into a single
     command.
  update  - syntax: (update <variable name> <s-expression>).
     This command updates the environment
     so that <variable name> will be associated with
     the value of <s-expression> in the new environment.
     If <variable name> is not found in the current environment,
     an error is signaled. This command returns a pair
     (<variable name> <value>) where <value> is a string
     or an integer obtained by evaluating <s-expression>.
  insert  - syntax: (insert . <regular expr seq>).
     This command insert the string represented by <regular expr seq>
     in the buffer. It returns a pair of nil and the current
     environemnt.
     Note that the use of dot pair in the syntax,
     which means that we need no parentheses in the arguments.

Adding Your Own Commands:
 You can add your own command by 'rewrite-method-def' whose syntax is
 given as follows:
   (rewrite-method-def <command name> <lambda expression>),
 where <lambda expression> is a lambda expression with two arguments.
 The first one represents the argument list for <command name>.
 The second one represents the environment of the rewriting system.
