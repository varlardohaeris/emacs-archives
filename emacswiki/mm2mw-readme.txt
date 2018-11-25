To use this, just find the base name of your MoinMoin Wiki and the
base name of your new MediaWiki installations, and feed them to
mm2mw-wiki.  Tested only on Emacs 22, requires at least a url library
with the ability to do HTTP POSTs.  Check the *Messages* buffer to
see how it did, per page.

My MoinMoin wiki was v1.2.x, so I have absolutely no idea if it will
work on later versions.  This might give somebody a good start,
though.  I don't claim that it supports everything; it definitely
does not handle attachments correctly.  There are probably better
ways to do everything but I built this in a couple of days from
scratch for my own purposes, and if God is good to me I will not have
to do it again.

I ripped the wikiname regexp from oddmuse.el and the url HTTP POST
code from an example on EmacsWiki; beyond that I wrote this on my
own.
