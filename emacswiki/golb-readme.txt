Golb is a very simple homepage and weblog generator. Basically it
provides a simple function which creates an html file from
templates for you, and possibly also a weblog index file and rss
feed.

When running M-x bolg in a file, this will create an associated
.html file. Your file itself should consist only of the body of a
real HTML file. Ideally, it should begin with the title of the page
in <h1>...</h1> tags, followed by a lead paragraph in <p>...</p>
tags. The file can be ended by a time stamp, which is not
considered to be part of the body itself. These parts are then
aggregated into an html file.

Files considered to be weblog files should be named YYYY-MM-DD.*.
They are treated like other files, except that they also cause an
index file and rss file to be generated.

To use it, put `golb' in your `after-save-hook'.

Well, read the docstrings and just try it.
