Commentary:
tumbl.el allows posting to tumblr.com through the api the
tumblr.com service provides. See http://www.tumblr.com/api for more
info about the tumblr.com api service.

Installation:
You will need to register with tumblr.com before use.
tumbl.el require http-post-simple which can be found at
http://www.emacswiki.org/cgi-bin/wiki/http-post-simple.el

Description:
Interactive functions:
tumbl-post-buffer-text
    Post complete buffer text to your tumblr log as
    'regular' post type. Prompts for buffer name and title
tumbl-post-region-text
    Post region as 'regular' post. Prompts for title
tumbl-post-region-as-quote
    Post region as tumblr quote. Prompts for quote source.
tumbl-post-region-as-conversation
    Post region as tumblr conversation. Prompts for title.
tumbl-post-link-url
    Post new link. The url is captured from the current
    region. Prompts for link name and description
tumbl-post-link-text
    Post new link. The link name is captured from the current
    region. Prompts for link URL and description
