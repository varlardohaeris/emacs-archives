There has been a tendency among Scheme teachers to use brackets at
some places in the Scheme syntax to help newcomers to read the
language. Some experienced Scheme users claim that this makes code
less readable, and I agree. If you do, too, this mode will protect
you from the malicious bracket invasion.

See http://www.forcix.cx/weblog/2006-05-07.html for a longer
explanation.

To use, simply M-x bracketphobia in a file with brackets, or add
`bracketphobia-auto' to your `find-file-hook'.

There's also M-x bracketphobia-hide, but that is only usable for
viewing, and I haven't found out how to turn it off yet :-)
