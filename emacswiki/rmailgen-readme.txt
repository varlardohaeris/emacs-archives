This package generates guesses for RMAIL output files when `o'
(`rmail-output-to-rmail-file') is hit in RMAIL.  When replying to mail
messages (`r') it will generate archive file names (i.e., the FCC, file
carbon-copy, file names that appear in *mail* buffers) if
`rmailgen-archive-file-name' is non-nil (the default).  Both of
these are done using the header of the current RMAIL message.  See Example
Installation below.

This package also allows user to set a default RMAIL mail directory, see
`rmailgen-default-directory'.

Specifically, this package parses the account names in the "From:" and
"To:" lines of the original RMAIL message.  It does this by placing a lisp
expression in `rmail-output-file-alist'.  The expression-driven form of
`rmail-output-file-alist' requires GNU Emacs version 19.17 or later.

Note that `C-o' (`rmail-output'), used for Un*x output format, does not
use `rmail-output-file-alist' so in that case no name is automatically
generated.  However, `o' (`rmail-output-to-rmail-file') is smart enough to
distinguish between Un*x and BABYL format in pre-existing folders.
Therefore, there is really no need to use C-o for appending to a
pre-existing folder (`C-o' is still very useful for outputing a message in
plain Un*x format -- in case you don't want a BABYL header stuck on it).
