This program provides some interactive functions in order that we can
easily request the text translation between English and Japanese to
multiple Web translation services. These functions were implemented
using `url-retrieve', so the responses will be displayed asynchronously
in the popup window.

This program is tested on GNU Emacs 22, 23.


Installation:

First, save this file as trans-ej.el and byte-compile in a directory
that is listed in load-path.

Put the following in your .emacs file:

  (require 'trans-ej)

To translate the buffer text, select region you desire, and just type as:

  M-x trans-ej

Have fun!
