COMMENTARY:

=================================================================
DESCRIPTION:
google-define-redux provides extensions for google-define.el
Extends google-define with fontification of definition buffer.
No longer relies on `with-output-to-temp-buffer'
Adds parsing tokens for programatically extracting definitions.

FUNCTIONS:►►►
`google-define-get-command', `google-define-parse-buffer', `google-define',
`google-define-font-lock', `google-define-kill-def-buffers',
`google-define-word-at-point',
`google-define-make-query-url', `google-define-clean-string',
`google-define-button-action', `google-define-button-do-xref',
`google-define-insert-xref-button', `google-define-find-next-heading',
`google-define-make-heading-replacement', `google-define-find-headings',
`google-define-related-language-url', `google-define-find-itemized',
`google-define-find-next-url-in-heading',
`google-define-parse-related-languages',
`google-define-related-language-xref', `%google-define-clean-term-for-split',
`%google-define-find-next-related-language-xref',
`%google-define-find-itemized-regexp-for-search-term',
`%google-define-verify-query-url-lang-params',
`%google-define-verify-query-url-xref-type-params',
`%google-define-set-url-current-object'
FUNCTIONS:◄◄◄

MACROS:

METHODS:

CLASSES:

FACES:
`gg-def-base', `gg-def-delim', `gg-def-defined', `gg-def-num',
`gg-def-inition', `gg-def-heading',

:BUTTONS
`google-define-button', `google-related-lang-button',

CONSTANTS:
`*google-define-html-entry-table*',

VARIABLES:
`*google-define-view-map*', `*google-define-get-buffer*',
`*google-define-buffer-suffix*', `*get-google-defined*',
`*regexp-google-defined-fontlock*', `*google-define-redux-xrefs*',
`*regexp-google-define-headings*',
`*regexp-google-define-dictionary-heading-tag*',
`*regexp-google-define-itemized*',
`*regexp-google-define-next-url-in-heading*',
`*regexp-google-define-source-ref*',

GROUPS:
`google-define-redux', `google-define-redux-faces',

ALIASED/ADVISED/SUBST'D:

DEPRECATED:

RENAMED:

MOVED:
`google-define-get-command-TEST'            -> mon-testme-utils.el

TODO:
Integrate Wordnet features.
:SEE (URL `http://wordnet.cs.princeton.edu')

Google's "define:<SOME-WORD>" returns wordnet definitions.  These appear in
the `*google-define-get-buffer*' returned by `google-define-get-command' as:
<a href="/url?q=http://wordnetweb.princeton.edu/perl/webwn{WORDNET-PARAMS}>
It might be interesting to integrate/adapt William Xu's wordnet interface:
:SEE (URL `http://xwl.appspot.com/ref/wordnet.el')
:SEE (URL `http://github.com/xwl/xwl-elisp/raw/master/wordnet.el')
Or maybe Henry G. Weller's adaptation of above for use w/ `org-mode':
:SEE (URL `http://www.emacswiki.org/emacs/wn-org.el')
using the REST

NOTES:
The required functions listed below are also provided in a dedicated library
with :FILE google-define-redux-supplemental.el
:SEE (URL `http://www.emacswiki.org/emacs/google-define-redux-supplemental.el')
Though, that package not always be current and it is preferred/recommended to
load the required packages instead.

SNIPPETS:

REQUIRES:
font-lock.el

:REQUIRES `mon-help-temp-docstring-display' <- :FILE mon-doc-help-utils.el
:REQUIRED-BY `google-define'

`*mon-help-docstring-help-bffr*'            <- :FILE mon-doc-help-utils.el
:REQUIRED-BY `mon-help-temp-docstring-display'

:REQUIRES `mon-help-KEY-tag'                <- :FILE mon-doc-help-utils.el
:REQUIRED-BY `gg-def-base'

:REQUIRES `mon-string-justify-left'         <- :FILE mon-utils.el
:REQUIRED-BY `google-define-parse-buffer'

:REQUIRES `mon-g2be'                        <- :FILE mon-utils.el
:REQUIRED-BY `google-define-font-lock', `google-define-parse-buffer'

:REQUIRES `mon-buffer-exists-p'             <- :FILE mon-utils.el
:REQUIRED-BY `google-define-kill-def-buffers'

:REQUIRES `mon-buffer-exists-so-kill'       <- :FILE mon-utils.el
:REQUIRED-BY `google-define-kill-def-buffers'

THIRD-PARTY-CODE:
Following are the modifications made to Jeremy English's google-define.el
:SEE (URL `http://www.emacswiki.org/emacs/google-define')
:SEE (URL `http://www.emacswiki.org/emacs/google-define.el')
SEE BOF for original versions.
These are provided verbatim:
`google-define-word-at-point'
`*google-define-html-entry-table*'
Following functions are modified:
`google-define-get-command'
`google-define-parse-buffer'
`google-define'
Following functions were replaced with inline forms:
`google-define-replace-unicode'
`google-define-replace-html'
`google-define-replace-string'
`google-define-ascii-entry'
`google-define-number-entry'

URL: https://github.com/mon-key/mon-emacs/raw/master/emacs-load-files/naf-mode/google-define-redux.el
EMACSWIKI-URL: http://www.emacswiki.org/emacs/google-define-redux.el
FIRST-PUBLISHED: <Timestamp: #{2010-02-04T21:39:15-05:00Z}#{10055} - by MON>

EMACSWIKI:

FILE-CREATED:
<Timestamp: #{2010-02-03T13:41:11-05:00Z}#{10053} - by MON>

=================================================================
