This library provides basic functions to manipulate RSS file.
Currently support only version 2.0 RSS. Maybe other version
of RSS will support in future. I think these API will not
change:
 (rss-read)
 (rss-read-file file)
 (rss-write rss &optional no-newlines)
 (rss-write-file rss file &optional no-newlines)
 (make-rss &optional version)
 (rss-channel rss node ...)
 (rss-add-item rss title link description)
 (rss-add-item rss '(attr node ...))
 (rss-image rss title url link)
 (rss-image rss '(attr node ...))
 (rss-textInput rss title name link)
 (rss-textInput rss '(attr node ...))

Put this file into your load-path and the following into your ~/.emacs:
  (require 'rss)
