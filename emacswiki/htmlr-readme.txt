Provides functions to render HTML in a buffer, and a filter
function that does the right thing when connected to a network
stream.

If you want to contribute see the HomePage and the TODOs in the code

History
0.0.7
 - bug fix: wrap/fill even if there are no tags in the text
0.0.6
 - support for pre, code tags
 - several fixes for the generation of newlines, including
   consolidation of multiple blank lines for cleaner-looking output
 - added new behavioral customization variables
 - code cleanup: FSF elisp style guide compliance fixes
0.0.5
 - support for dl dd dt tags
 - fixes new line problems
 - other fixes
0.0.4
 - support for h1..h6, ol tags
 - more style variables
 - highlight for link and img tags
 - more fixes
0.0.3
 - change the behaviour of the parser. It now uses the content
   of the stack for the transitions and not last-tag.
 - as a result, all tags but empty tags (img, hr ...) and closing
   tags must be pushed.
 - transition definitions are more compact because we don't need
   transition from closing tag anymore
 - stack now contains the position of the beginning of the tag
 - introduce `htmlr-last-wrap-pos' variable to keep track of what
   has already been wrapped
 - bugs treat space around <img> <a> <b> etc.. a trick solution
   but a \n if we need a space because \n are replace by space by
   wrap while space are erased.
 - add the bullet list definition for sublist + * - etc...
 - other small fixes

0.0.2
 - added whatever tag to limit the complexity of the definition
   of `htmlr-transitions'.
 - added support for links [src text]. better highlight needed
 - added support for images {src (alt | name)} better highlight needed
 - added support for i, em, hr, br, tags
