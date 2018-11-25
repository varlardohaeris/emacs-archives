Poorman's implementation of combined expressions.
Combined expressions are combinations of regular expressions and balanced expressions.
You can use search-forward-cexp for searching combined expressions.
Some clumsy way of storing the match-data and the balanced expressions is provided.
That is just a test implementation.
Example:
(setq var-match-data (search-forward-cexp "foo\\!(\\`(\\([[:alpha:]][[:alnum:]]*\\)\\(,[[:alpha:]][[:alnum:]]*\\)*)\\'\\!)"))
This combined expression matches the following string:
foo(x,y34,zoo)
Investigate the match data:
(set-match-data var-match-data)
For an example get the function name:
(match-string-no-properties 0)
And get the balanced expression inclusive the delimiters which are assured to be `(' and `)' in this example:
(match-string-no-properties 1)

Changes:

2012-01-25: Upload.
